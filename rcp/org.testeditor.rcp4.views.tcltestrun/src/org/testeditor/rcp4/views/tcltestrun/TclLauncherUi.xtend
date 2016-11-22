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
import java.io.FileFilter
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
import org.eclipse.emf.common.util.URI
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.jdt.junit.JUnitCore
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.jface.viewers.LabelProvider
import org.eclipse.jface.window.Window
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.dialogs.ElementListSelectionDialog
import org.slf4j.LoggerFactory
import org.testeditor.dsl.common.ui.utils.ProgressMonitorRunner
import org.testeditor.dsl.common.ui.workbench.PartHelper
import org.testeditor.dsl.common.util.EclipseContextHelper
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
import org.eclipse.e4.ui.workbench.modeling.EPartService
import org.eclipse.swt.widgets.Display
import org.testeditor.rcp4.tcltestrun.LaunchResult

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

	override boolean launch(IStructuredSelection selection, IProject project, String mode, boolean parameterize) {
		eclipseContextHelper.eclipseContext.set(TclLauncherUi, this)
		val options = newHashMap
		tslIndex = indexHelper.createTestCaseIndex()
		if (project.getFile("build.gradle").exists) {
			return launchTest(
				new TestLaunchInformation(createGradleTestCasesList(selection), project, gradleLauncher, options))
		}
		if (project.getFile("pom.xml").exists) {
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

	def boolean launchTest(TestLaunchInformation testLaunchInformation) {
		storeTestParameterAsLastTestExecution(testLaunchInformation)
		logger.info("Trying to launch launcherClass='{}' test execution for elementId='{}' in project='{}'",
			testLaunchInformation.launcher.class.simpleName, testLaunchInformation.testCasesCommaList?.head,
			testLaunchInformation.project)
		progressRunner.run([ monitor |
			if (testLaunchInformation.testCasesCommaList != null) {
				monitor.beginTask("Test execution: " + testLaunchInformation.testCasesCommaList.head,
					IProgressMonitor.UNKNOWN)
			} else {
				monitor.beginTask("Test execution: " + testLaunchInformation.project.name, IProgressMonitor.UNKNOWN)
			}
			val con = consoleFactory.createAndShowConsole
			val list = testLaunchInformation.testCasesCommaList ?: #[]
			val execLog = testExecutionManager.createTestExecutionLog(list)
			partHelper.showView(TEST_EXECUTION_RESULT_VIEW)
			val viewPart = eclipseContextHelper.eclipseContext.get(EPartService).findPart(TEST_EXECUTION_RESULT_VIEW)
			val teExecView = viewPart.object as TestExecutionLogViewPart
			Display.^default.syncExec[teExecView?.showLog(execLog)]
			var LaunchResult result = null
			var output = new TeeOutputStream(con.newOutputStream, testExecutionManager.createOutputStreamFor(execLog))
			try {
				result = testLaunchInformation.launcher.launchTest(testLaunchInformation.testCasesCommaList,
					testLaunchInformation.project, monitor, output, testLaunchInformation.options)
			} finally {
				if (output !== null) {
					output.close
					output=null
				}
			}
			testLaunchInformation.project.refreshLocal(IProject.DEPTH_INFINITE, monitor)
			if (result.expectedFileRoot == null) {
				logger.error("resulting expectedFile must not be null")
			} else {
				safeUpdateJunitTestView(result.expectedFileRoot, testLaunchInformation.project.name)
			}
			monitor.done
			partHelper.showView(TEST_EXECUTION_RESULT_VIEW)
		])
		return true
	}

	def storeTestParameterAsLastTestExecution(TestLaunchInformation testLaunchInformation) {
		logger.debug("Storing test execution as last launch")
		eclipseContextHelper.eclipseContext.set(TestLaunchInformation, testLaunchInformation)
	}

	def List<String> createGradleTestCasesList(IStructuredSelection selection) {
		val result = new ArrayList<String>()
		result += selection.toList.filter(IResource).map [ resource |
			if (resource instanceof IFolder) {
				val javaElement = resource.getAdapter(IJavaElement) as IJavaElement
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

	def Iterable<String> getTestCaseListFromSelection(Object sel) {
		if (sel instanceof IFolder) {
			return sel.testCasesFromFolder
		} else {
			if (sel instanceof IResource) {
				if (sel.fileExtension.equalsIgnoreCase("tsl")) {
					val uri = URI.createPlatformResourceURI(sel.locationURI.toString, true).deresolve(
						URI.createPlatformResourceURI(sel.project.locationURI.toString, true))
					val secondURI = URI.createPlatformResourceURI(uri.toString, true)
					return tslIndex.get(secondURI).map[it.model.package + "." + it.name]
				} else {
					val launchShortcutUtil = tclInjectorProvider.get.getInstance(LaunchShortcutUtil)
					return #[launchShortcutUtil.getQualifiedNameForTestInTcl(sel).toString]
				}
			}
		}
	}

	def List<String> getTestCasesFromFolder(IFolder folder) {
		val result = newArrayList()
		val launchShortcutUtil = tclInjectorProvider.get.getInstance(LaunchShortcutUtil)
		for (IResource res : folder.members) {
			if (res instanceof IFile) {
				if (res.fileExtension.equalsIgnoreCase("tcl")) {
					result.add(launchShortcutUtil.getQualifiedNameForTestInTcl(res).toString)
				}
			}
			if (res instanceof IFolder) {
				result.addAll(res.testCasesFromFolder)
			}
		}
		return result
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
		if (xmlResults != null && xmlResults.length > 0) {
			testResultFileWriter.writeTestResultFile(projectName, resultFile, xmlResults)
		} else {
			testResultFileWriter.writeErrorFile(projectName, resultFile)
		}
		JUnitCore.importTestRunSession(resultFile)
		partHelper.showView(RESULT_VIEW)
	}

}
