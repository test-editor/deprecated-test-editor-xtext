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
import java.util.Map
import java.util.concurrent.atomic.AtomicReference
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
import java.io.FileFilter
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import org.w3c.dom.Attr
import org.w3c.dom.Document
import org.testeditor.rcp4.tcltestrun.TclInjectorProvider
import org.testeditor.tcl.dsl.ui.testlaunch.LaunchShortcutUtil
import org.eclipse.core.resources.IResource
import java.util.List
import org.w3c.dom.Node

class TclLauncherUi implements Launcher {
	static val logger = LoggerFactory.getLogger(TclLauncherUi)

	@Inject ProgressMonitorRunner progressRunner
	@Inject TclMavenLauncher mavenLauncher
	@Inject TclGradleLauncher gradleLauncher
	var LaunchShortcutUtil launchShortcutUtil // since this class itself is instanciated by e4, this attribute has to be injected manually

	@Inject
	new(TclInjectorProvider tclInjectorProvider) {
		launchShortcutUtil = tclInjectorProvider.get.getInstance(LaunchShortcutUtil)
	}

	override boolean launch(IStructuredSelection selection, IProject project, String mode, boolean parameterize) {
		val options = newHashMap
		if (project.getFile("build.gradle").exists) {
			return launchTest(selection, project, gradleLauncher, options)
		}
		if (project.getFile("pom.xml").exists) {
			if (parameterize) {
				val profile = selectMavenProfile(project)
				if (profile == null) {
					return true // should an option always be selected?
				}
				options.put(TclMavenLauncher.PROFILE, profile)
			}
			return launchTest(selection, project, mavenLauncher, options)
		}
		logger.warn("gradle based launching test for tcl element='{}' failed, since file='build.gradle' was not found.",
			selection.firstElement)
		return false
	}

	private def String selectMavenProfile(IProject project) {
		val mavenProfiles = project.collectMavenProfilesWithProgress
		val dialog = new ElementListSelectionDialog(PlatformUI.workbench.activeWorkbenchWindow.shell, new LabelProvider)
		dialog.setElements(mavenProfiles)
		dialog.setTitle("Which maven profile should be used?")
		if (dialog.open == Window.OK) {
			return dialog.result.head.toString
		} else {
			return null // cancelled
		}
	}

	private def Iterable<String> collectMavenProfilesWithProgress(IProject project) {
		val result = new AtomicReference<Iterable<String>>
		progressRunner.run([ monitor |
			monitor.beginTask("Collect maven profiles", IProgressMonitor.UNKNOWN)
			result.set(mavenLauncher.getProfiles(project))
			monitor.done
		])
		return result.get
	}

	private def boolean launchTest(IStructuredSelection selection, IProject project, TclLauncher launcher,
		Map<String, Object> options) {
		logger.info("Trying to launch launcherClass='{}' test execution for elementId='{}' in project='{}'",
			launcher.class.simpleName, selection.firstElement, project)
		progressRunner.run([ monitor |
			monitor.beginTask("Test execution: " + selection.firstElement, IProgressMonitor.UNKNOWN)
			val testCasesCommaList = createTestCasesList(selection)
			val result = launcher.launchTest(testCasesCommaList, project, monitor, options)
			project.refreshLocal(IProject.DEPTH_INFINITE, monitor)
			if (result.expectedFileRoot == null) {
				logger.error("resulting expectedFile must not be null")
			} else {
				safeUpdateJunitTestView(result.expectedFileRoot, project.name)
			}
			monitor.done
		])
		return true
	}

	def List<String> createTestCasesList(IStructuredSelection selection) {
		if (selection.size > 1) {
			return selection.toList.map[launchShortcutUtil.getQualifiedNameForTestInTcl(it as IResource).toString]
		} else {
			return #[launchShortcutUtil.getQualifiedNameForTestInTcl(selection.firstElement as IResource).toString]
		}
	}

	/** 
	 * provide test result file (either the one created by the test run, or, if absent/on error 
	 * a default error file) that is imported into junit (and thus displayed) 
	 * */
	private def void safeUpdateJunitTestView(File expectedFileRoot, String projectName) {
		logger.debug("Test result parentPir={}", expectedFileRoot)
		val xmlResults = expectedFileRoot.listFiles(new FileFilter() {

			override accept(File pathname) {
				return pathname.isFile && pathname.name.endsWith(".xml")
			}

		})
		val resultFile = new File(expectedFileRoot, "te-testCompose.xml")
		if (xmlResults.length > 0) {
			writeTestResultFile(projectName, resultFile, xmlResults)
		} else {
			writeErrorFile(projectName, resultFile)
		}
		JUnitCore.importTestRunSession(resultFile)
	}

	def void writeTestResultFile(String projectName, File resultFile, File[] xmlResults) {
		val docBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder
		val resultDoc = docBuilder.newDocument
		val testRun = resultDoc.createElement("testrun")
		resultDoc.appendChild(testRun)
		var testCount = 0;
		var failureCount = 0;
		var errorsCount = 0;
		var ignoreCount = 0;
		for (file : xmlResults) {
			val suiteDoc = docBuilder.parse(file)
			val nodeList = suiteDoc.childNodes
			for (var i = 0; i < nodeList.length; i++) {
				if (nodeList.item(i).nodeName.equals("testsuite")) {
					testRun.appendChild(resultDoc.importNode(nodeList.item(i), true))
					testCount += getIntFromAttribute(nodeList.item(i), "tests")
					failureCount += getIntFromAttribute(nodeList.item(i), "failures")
					errorsCount += getIntFromAttribute(nodeList.item(i), "errors")
					ignoreCount += getIntFromAttribute(nodeList.item(i), "skipped")
				}
			}
		}
		testRun.attributeNode = resultDoc.createAttribute("name", "java")
		testRun.attributeNode = resultDoc.createAttribute("project", projectName)
		testRun.attributeNode = resultDoc.createAttribute("tests", Integer.toString(testCount))
		testRun.attributeNode = resultDoc.createAttribute("started", Integer.toString(testCount))
		testRun.attributeNode = resultDoc.createAttribute("failures", Integer.toString(failureCount))
		testRun.attributeNode = resultDoc.createAttribute("errors", Integer.toString(errorsCount))
		testRun.attributeNode = resultDoc.createAttribute("ignored", Integer.toString(ignoreCount))
		val transformerFactory = TransformerFactory.newInstance();
		val transformer = transformerFactory.newTransformer();
		val source = new DOMSource(resultDoc);
		transformer.transform(source, new StreamResult(resultFile));
	}

	def int getIntFromAttribute(Node node, String attributeName) {
		Integer.parseInt(node.attributes.getNamedItem(attributeName).nodeValue)
	}

	def Attr createAttribute(Document doc, String attributeName, String attributeValue) {
		var result = doc.createAttribute(attributeName)
		result.value = attributeValue
		return result
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
