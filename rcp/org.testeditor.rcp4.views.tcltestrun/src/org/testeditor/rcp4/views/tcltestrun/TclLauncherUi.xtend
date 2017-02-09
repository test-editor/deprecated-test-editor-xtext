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
import java.nio.file.Files
import java.util.ArrayList
import java.util.List
import java.util.Map
import java.util.concurrent.atomic.AtomicReference
import javax.inject.Inject
import org.apache.commons.io.output.TeeOutputStream
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IFolder
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.Status
import org.eclipse.core.runtime.jobs.Job
import org.eclipse.e4.ui.workbench.modeling.EPartService
import org.eclipse.emf.common.util.URI
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.jdt.junit.JUnitCore
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.jface.viewers.LabelProvider
import org.eclipse.jface.window.Window
import org.eclipse.swt.widgets.Display
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.dialogs.ElementListSelectionDialog
import org.slf4j.LoggerFactory
import org.testeditor.dsl.common.ide.util.FileUtils
import org.testeditor.dsl.common.ui.utils.ProgressMonitorRunner
import org.testeditor.dsl.common.ui.utils.WorkbenchHelper
import org.testeditor.dsl.common.ui.workbench.PartHelper
import org.testeditor.dsl.common.util.EclipseContextHelper
import org.testeditor.dsl.common.util.MavenExecutor
import org.testeditor.rcp4.tcltestrun.LaunchResult
import org.testeditor.rcp4.tcltestrun.TclGradleLauncher
import org.testeditor.rcp4.tcltestrun.TclMavenLauncher
import org.testeditor.rcp4.views.tcltestrun.console.TCLConsoleFactory
import org.testeditor.rcp4.views.tcltestrun.console.TestExecutionLogViewPart
import org.testeditor.rcp4.views.tcltestrun.model.TestExecutionManager
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.dsl.ui.testlaunch.LaunchShortcutUtil
import org.testeditor.tcl.dsl.ui.testlaunch.Launcher
import org.testeditor.tcl.dsl.ui.util.TclIndexHelper
import org.testeditor.tcl.dsl.ui.util.TclInjectorProvider

class TclLauncherUi implements Launcher {

	static val RESULT_VIEW = "org.eclipse.jdt.junit.ResultView"
	static val TEST_EXECUTION_RESULT_VIEW = "org.testeditor.rcp4.views.tcltestrun.part.testexecutionconsole"
	static val logger = LoggerFactory.getLogger(TclLauncherUi)

	@Inject ProgressMonitorRunner progressRunner
	@Inject TclMavenLauncher mavenLauncher
	@Inject TclGradleLauncher gradleLauncher
	@Inject TestResultFileWriter testResultFileWriter
	@Inject TclIndexHelper indexHelper
	@Inject TclInjectorProvider tclInjectorProvider
	Map<URI, ArrayList<TestCase>> tslIndex
	@Inject TCLConsoleFactory consoleFactory
	@Inject PartHelper partHelper
	@Inject EclipseContextHelper eclipseContextHelper
	@Inject TestExecutionManager testExecutionManager
	@Inject WorkbenchHelper workbenchHelper

	override boolean launch(IStructuredSelection selection, IProject project, String mode, boolean parameterize) {
		eclipseContextHelper.eclipseContext.set(TclLauncherUi, this)
		val options = newHashMap
		tslIndex = indexHelper.createTestCaseIndex
		if (project.getFile("build.gradle").exists) {
			return launchTest(
				new TestLaunchInformation(createGradleTestCasesList(selection), project, gradleLauncher, options))
		}
		if (project.getFile("pom.xml").exists && continueWithMaven) {
			if (parameterize) {
				val profile = selectMavenProfile(project)
				if (profile == null) {
					return true // should an option always be selected?
				}
				options.put(TclMavenLauncher.PROFILE, profile)
			}
			return launchTest(
				new TestLaunchInformation(createTestCasesList(selection), project, mavenLauncher, options))
		}
		logger.warn("gradle based launching test for tcl element='{}' failed, since file='build.gradle' was not found.",
			selection.firstElement)
		return false
	}

	def boolean launchTest(TestLaunchInformation testLaunchInformation) {
		storeTestParameterAsLastTestExecution(testLaunchInformation)
		logger.info("Trying to launch launcherClass='{}' test execution for elementId='{}' in project='{}'",
			testLaunchInformation.launcher.class.simpleName, testLaunchInformation.testCasesCommaList?.head,
			testLaunchInformation.project)
		val job = new Job("Test execution") {

			override protected run(IProgressMonitor monitor) {
				if (testLaunchInformation.testCasesCommaList != null) {
					monitor.beginTask("Test execution: " + testLaunchInformation.testCasesCommaList.head,
						IProgressMonitor.UNKNOWN)
				} else {
					monitor.beginTask("Test execution: " + testLaunchInformation.project.name, IProgressMonitor.UNKNOWN)
				}
				val con = consoleFactory.createAndShowConsole
				val list = testLaunchInformation.testCasesCommaList ?: #[]
				partHelper.showView(TEST_EXECUTION_RESULT_VIEW)
				val execLog = testExecutionManager.createTestExecutionLog(list)
				val testResultDir = testExecutionManager.createTestlogDirectoryFor(execLog)
				val logFileStream = new FileOutputStream(new File(testResultDir, "testrun.log"))
				val output = new TeeOutputStream(con.newOutputStream, logFileStream)
				var LaunchResult result = null
				try {
					result = testLaunchInformation.launcher.launchTest(testLaunchInformation.testCasesCommaList,
						testLaunchInformation.project, monitor, output, testLaunchInformation.options)
				} finally {
					output.close
				}
				testLaunchInformation.project.refreshLocal(IProject.DEPTH_INFINITE, monitor)
				if (result.expectedFileRoot == null) {
					logger.error("resulting expectedFile must not be null")
				} else {
					safeUpdateJunitTestView(result.expectedFileRoot, testLaunchInformation.project.name)
					collectTestResultFiles(result.expectedFileRoot, testResultDir)
				}
				monitor.done
				partHelper.showView(TEST_EXECUTION_RESULT_VIEW)
				Display.^default.syncExec[testExecutionLogViewPart?.showLog(execLog)]
				return Status.OK_STATUS
			}

		}
		job.user = true
		job.schedule
		return true
	}

	def void storeTestParameterAsLastTestExecution(TestLaunchInformation testLaunchInformation) {
		logger.debug("Storing test execution as last launch")
		eclipseContextHelper.eclipseContext.set(TestLaunchInformation, testLaunchInformation)
	}

	def List<String> createGradleTestCasesList(IStructuredSelection selection) {
		val result = newArrayList
		result += selection.toList.filter(IResource).map [ resource |
			if (resource instanceof IFolder) {
				val javaElement = resource.getAdapter(IJavaElement)
				if (javaElement != null) {
					return javaElement.elementName + "*"
				} else {
					logger.warn("selected element = {} is not a test exeutuable", resource.name)
					return null
				}

			} else {
				return tclInjectorProvider.get.getInstance(LaunchShortcutUtil).getQualifiedNameForTestInTcl(resource).
					toString
			}
		].filterNull

		return result

	}

	def List<String> createTestCasesList(IStructuredSelection selection) {
		if (selection.size > 1) {
			return selection.toList.map[testCaseListFromSelection].flatten.toList
		} else {
			if (selection.firstElement instanceof IProject) {
				return null
			}
			return selection.firstElement.testCaseListFromSelection.toList
		}
	}

	def Iterable<String> getTestCasesFromFolder(IFolder folder) {
		val extension launchShortcutUtil = tclInjectorProvider.get.getInstance(LaunchShortcutUtil)
		val members = folder.members
		val tclFiles = members.filter(IFile).filter[fileExtension.equalsIgnoreCase('tcl')].map [
			qualifiedNameForTestInTcl.toString
		]
		val fromRecursionIntoSubfolders = members.filter(IFolder).map[testCasesFromFolder].flatten
		return tclFiles + fromRecursionIntoSubfolders
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

	private def dispatch Iterable<String> getTestCaseListFromSelection(IFolder folder) {
		return folder.testCasesFromFolder
	}

	private def dispatch Iterable<String> getTestCaseListFromSelection(IResource resource) {
		if (resource.fileExtension.equalsIgnoreCase("tsl")) {
			val uri = URI.createPlatformResourceURI(resource.locationURI.toString, true).deresolve(
				URI.createPlatformResourceURI(resource.project.locationURI.toString, true))
			val secondURI = URI.createPlatformResourceURI(uri.toString, true)
			return tslIndex.get(secondURI).map[it.model.package + "." + it.name]
		} else {
			val launchShortcutUtil = tclInjectorProvider.get.getInstance(LaunchShortcutUtil)
			return #[launchShortcutUtil.getQualifiedNameForTestInTcl(resource).toString]
		}
	}

	private def dispatch Iterable<String> getTestCaseListFromSelection(Object object) {
		return emptyList
	}

	private def Iterable<String> collectMavenProfilesWithProgress(IProject project) {
		val result = new AtomicReference<Iterable<String>>
		progressRunner.run [
			beginTask("Collect maven profiles", IProgressMonitor.UNKNOWN)
			result.set(mavenLauncher.getProfiles(project))
			done
		]
		return result.get
	}

	/** 
	 * provide test result file (either the one created by the test run, or, if absent/on error 
	 * a default error file) that is imported into junit (and thus displayed) 
	 * */
	private def void safeUpdateJunitTestView(File expectedFileRoot, String projectName) {
		logger.debug("Test result parentPir={}", expectedFileRoot)
		val xmlResults = expectedFileRoot.listFiles[it.isFile && it.name.endsWith(".xml")]
		val resultFile = new File(expectedFileRoot, "te-testCompose.xml")
		if (xmlResults != null && xmlResults.length > 0) {
			testResultFileWriter.writeTestResultFile(projectName, resultFile, xmlResults)
		} else {
			testResultFileWriter.writeErrorFile(projectName, resultFile)
		}
		JUnitCore.importTestRunSession(resultFile)
		partHelper.showView(RESULT_VIEW)
	}

	private def collectTestResultFiles(File resultRoot, File testResultDir) {
		Files.copy(new File(resultRoot, "te-testCompose.xml").toPath, new File(testResultDir, "testSummary.xml").toPath)
		val screenshotDir = lookUpScreenShotDir(resultRoot)
		if (screenshotDir !== null) {
			FileUtils.copyFolder(screenshotDir, new File(testResultDir, "screenshots"))
		}
	}

	private def File lookUpScreenShotDir(File file) {
		return file.parentFile.parentFile.getDirWithName("screenshots")
	}

	private def File getDirWithName(File file, String name) {
		file.listFiles[it.name == name].head
	}

	private def TestExecutionLogViewPart getTestExecutionLogViewPart() {
		val viewPart = eclipseContextHelper.eclipseContext.get(EPartService).findPart(TEST_EXECUTION_RESULT_VIEW)
		return viewPart.object as TestExecutionLogViewPart
	}

	private def boolean continueWithMaven() {
		switch (mavenLauncher.mavenVersionValidity) {
			case no_maven:
				return workbenchHelper.answerYesNoErrorMessageDialog( "Configuration-Mismatch",
					'''
					No maven installation was found. Please install maven with minimum version «MavenExecutor.MAVEN_MIMIMUM_MAJOR_VERSION».«MavenExecutor.MAVEN_MIMIMUM_MINOR_VERSION»
					and set the variable «MavenExecutor.TE_MAVEN_HOME» to the path of the installation!
					
					Try to continue anyway?
					''')
			case wrong_version:
				return workbenchHelper.answerYesNoErrorMessageDialog( "Configuration-Mismatch",
					'''Maven is not available in the needed version for test-execution («MavenExecutor.MAVEN_MIMIMUM_MAJOR_VERSION».«MavenExecutor.MAVEN_MIMIMUM_MINOR_VERSION»). Continue anyway?''')
			case unknown_version:
				return workbenchHelper.answerYesNoErrorMessageDialog( "Configuration-Mismatch",
					"It was not possible to determine the current maven version. Testexecution may not be possible. Continue anyway?")
			default:
				return true
		}
	}

}
		