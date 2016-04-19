/*******************************************************************************
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 *******************************************************************************/
package org.testeditor.rcp4.views.tcltestrun

import com.google.common.io.Files
import java.io.File
import java.io.IOException
import java.nio.charset.StandardCharsets
import java.util.HashMap
import java.util.Map
import java.util.concurrent.CompletableFuture
import javax.inject.Inject
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jdt.junit.JUnitCore
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.jface.viewers.LabelProvider
import org.eclipse.jface.window.Window
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.dialogs.ElementListSelectionDialog
import org.slf4j.LoggerFactory
import org.testeditor.dsl.common.ui.utils.ProgressMonitorRunner
import org.testeditor.rcp4.tcltestrun.TclGradleLauncher
import org.testeditor.rcp4.tcltestrun.TclLauncher
import org.testeditor.rcp4.tcltestrun.TclMavenLauncher
import org.testeditor.tcl.dsl.ui.testlaunch.Launcher

class TclLauncherUi implements Launcher {
	static val logger = LoggerFactory.getLogger(TclLauncherUi)

	@Inject ProgressMonitorRunner progressRunner
	@Inject TclMavenLauncher mavenLauncher
	@Inject TclGradleLauncher gradleLauncher

	override boolean launch(IStructuredSelection selection, IProject project, String elementId, String mode,
		boolean parameterize) {
		val options = new HashMap<String, Object>
		if (project.getFile("build.gradle").exists) {
			return launchTest(selection, project, elementId, gradleLauncher, options)
		}
		if (project.getFile("pom.xml").exists) {
			if (parameterize) {
				val profile = uiCollectMavenProfile(project)
				if (profile == null) {
					return true // should an option always be selected?
				}
				options.putAll(#{TclMavenLauncher.PROFILE -> profile})
			}
			return launchTest(selection, project, elementId, mavenLauncher, options)
		}
		logger.warn("gradle based launching test for tcl element='{}' failed, since file='build.gradle' was not found.",
			elementId)
		logger.warn("maven based launching test for tcl element='{}' failed, since file='pom.xml' was not found.",
			elementId)
		return false
	}

	private def String uiCollectMavenProfile(IProject project) {
		val mavenProfiles = project.mavenGetProfilesWithUiFeedback
		val dialog = new ElementListSelectionDialog(PlatformUI.workbench.activeWorkbenchWindow.shell, new LabelProvider)
		dialog.setElements(mavenProfiles)
		dialog.setTitle("Which maven profile should be used?")
		if (dialog.open == Window.OK) {
			return dialog.result.get(0).toString
		} else {
			return null // cancelled
		}
	}

	private def Iterable<String> mavenGetProfilesWithUiFeedback(IProject project) {
		val result = new CompletableFuture<Iterable<String>>
		progressRunner.run([ monitor |
			monitor.beginTask("Collect maven profiles", IProgressMonitor.UNKNOWN)
			result.complete(mavenLauncher.getProfiles(project))
			monitor.done
		])
		return result.get
	}

	private def boolean launchTest(IStructuredSelection selection, IProject project, String elementId,
		TclLauncher launcher, Map<String, Object> options) {
		logger.info("Trying to launch launcherClass='{}' test execution for elementId='{}' in project='{}'",
			launcher.class.simpleName, elementId, project)
		progressRunner.run([ monitor |
			monitor.beginTask("Test execution: " + elementId, IProgressMonitor.UNKNOWN)
			val result = launcher.launchTest(selection, project, elementId, monitor, options)
			project.refreshLocal(IProject.DEPTH_INFINITE, monitor)
			if (result.expectedFile == null) {
				logger.error("resulting expectedFile must not be null")
			} else {
				safeUpdateJunitTestView(elementId, result.expectedFile)
			}
			monitor.done
		])
		return true
	}

	/** 
	 * provide test result file (either the one created by the test run, or, if absent/on error 
	 * a default error file) that is imported into junit (and thus displayed) 
	 * */
	private def void safeUpdateJunitTestView(String elementId, File expectedFile) {
		val parentFolder = new File(expectedFile.parent)
		if (!parentFolder.exists && !parentFolder.mkdirs) {
			logger.error('''failed to change into parentFolder='{}' ''', parentFolder.absolutePath)
		} else {
			val newFile = File.createTempFile(expectedFile.name, ".xml", parentFolder)
			if (expectedFile.exists) {
				try {
					Files.move(expectedFile, newFile)
				} catch (IOException e) {
					logger.error("error during storage of test run result " + expectedFile.path, e)
					writeErrorFile(elementId, newFile)
				}
			} else {
				writeErrorFile(elementId, newFile)
			}
			JUnitCore.importTestRunSession(newFile)
			try {
				java.nio.file.Files.delete(newFile.toPath)
			} catch (IOException e) {
				logger.warn("error during removal of obsolete test result " + newFile.path, e)
			}
		}
	}

	/**
	 * write a default error file for junit
	 */
	private def void writeErrorFile(String elementId, File file) {
		try {
			Files.write('''
			<?xml version="1.0" encoding="UTF-8"?>
			<testsuite name="«elementId»" tests="1" skipped="0" failures="0" errors="1" time="0.000">
			  <properties/>
			  <testcase name="execute" classname="«elementId»" time="0.000">
			    <error>
			      failed to execute test, please check your technical test setup 
			    </error>
			  </testcase>
			</testsuite>''', file, StandardCharsets.UTF_8);
		} catch (IOException e) {
			logger.error('''could not write test result error file='«file.path»' ''', e)
		}
	}

}
