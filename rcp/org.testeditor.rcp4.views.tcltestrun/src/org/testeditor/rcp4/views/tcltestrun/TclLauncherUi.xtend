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

import java.io.File
import java.io.FileOutputStream
import java.io.IOException
import java.io.OutputStream
import java.nio.charset.Charset
import java.nio.file.Files
import java.util.HashMap
import java.util.List
import java.util.Map
import javax.inject.Inject
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jdt.junit.JUnitCore
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.jface.viewers.LabelProvider
import org.eclipse.jface.window.Window
import org.eclipse.swt.widgets.Shell
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
				val mavenOptions = uiCollectMavenOptions
				if (mavenOptions == null) {
					return true // should an option always be selected?
				}
				options.putAll(mavenOptions)
			}
			return launchTest(selection, project, elementId, mavenLauncher, options)
		}
		logger.warn("gradle based launching test for tcl element='{}' failed, since file='build.gradle' was not found.",
			elementId)
		logger.warn("maven based launching test for tcl element='{}' failed, since file='pom.xml' was not found.",
			elementId)
		return false
	}

	private def Map<String, Object> uiCollectMavenOptions() {
		val mavenProfiles = mavenGetProfilesWithUiFeedback
		val dialog = new ElementListSelectionDialog(new Shell, new LabelProvider)
		dialog.setElements(mavenProfiles)
		dialog.setTitle("Which maven profile should be used?")
		if (dialog.open() == Window.OK) {
			val selectedProfile = dialog.result
			return #{TclMavenLauncher.PROFILE -> selectedProfile}
		} else {
			return null // cancelled
		}
	}

	private def List<String> mavenGetProfilesWithUiFeedback() {
		val List<String>[] container = newArrayOfSize(1)
		progressRunner.run([ monitor |
			monitor.beginTask("Collect maven profiles", IProgressMonitor.UNKNOWN)
			container.set(0, mavenLauncher.profiles)
			monitor.done
		])
		return container.head
	}

	private def boolean launchTest(IStructuredSelection selection, IProject project, String elementId,
		TclLauncher launcher, Map<String, Object> options) {
		logger.info("Trying to launch launcherClass='{}' test execution for elementId='{}' in project='{}'",
			launcher.class.simpleName, elementId, project)
		progressRunner.run([ monitor |
			monitor.beginTask("Test execution: " + elementId, IProgressMonitor.UNKNOWN)
			val result = launcher.launchTest(selection, project, elementId, monitor, options)
			project.refreshLocal(IProject.DEPTH_INFINITE, monitor)
			val expectedFile = (result.get(TclLauncher.EXPECTED_FILE) as File)
			if (expectedFile == null) {
				logger.error("resulting expectedFile must not be null")
			} else {
				updateJunitTestView(elementId, expectedFile)
			}
			monitor.done
		])
		return true
	}

	/** 
	 * provide test result file (either the one created by the test run, or, if absent/on error 
	 * a default error file) that is imported into junit (and thus displayed) 
	 * */
	private def void updateJunitTestView(String elementId, File expectedFile) {
		val newFile = File.createTempFile(expectedFile.name, ".xml", new File(expectedFile.parent))
		if (expectedFile.exists) {
			var OutputStream os = null
			try {
				os = new FileOutputStream(newFile)
				Files.copy(expectedFile.toPath, os)
				Files.delete(expectedFile.toPath)
			} catch (IOException e) {
				logger.error("error during storage of test run result " + expectedFile.path, e)
				writeErrorFile(elementId, newFile)
			} finally {
				try {
					os?.close
				} catch (IOException e) {
					logger.error('''error closing the outputstream during copy to newFile='«newFile.absolutePath»' ''',
						e)
				}
			}
		} else {
			writeErrorFile(elementId, newFile)
		}
		JUnitCore.importTestRunSession(newFile)
		try {
			Files.delete(newFile.toPath)
		} catch (IOException e) {
			logger.warn("error during removal of obsolete test result " + newFile.path, e)
		}
	}

	/**
	 * write a default error file for junit
	 */
	private def void writeErrorFile(String elementId, File file) {
		try {
			val os = new FileOutputStream(file)
			os.write('''
			<?xml version="1.0" encoding="UTF-8"?>
			<testsuite name="«elementId»" tests="1" skipped="0" failures="0" errors="1" time="0.000">
			  <properties/>
			  <testcase name="execute" classname="«elementId»" time="0.000">
			    <error>
			      failed to execute test, please check your technical test setup 
			    </error>
			  </testcase>
			</testsuite>'''.toString.getBytes(Charset.forName('UTF-8')))
			os.close
		} catch (IOException e) {
			logger.error('''could not write test result error file='«file.path»' ''', e)
		}
	}

}
