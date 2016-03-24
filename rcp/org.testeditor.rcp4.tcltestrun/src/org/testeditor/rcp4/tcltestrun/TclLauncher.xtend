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
package org.testeditor.rcp4.tcltestrun

import java.io.File
import javax.inject.Inject
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.Status
import org.eclipse.core.runtime.jobs.Job
import org.eclipse.jdt.junit.JUnitCore
import org.eclipse.jface.viewers.IStructuredSelection
import org.gradle.tooling.GradleConnectionException
import org.gradle.tooling.GradleConnector
import org.gradle.tooling.ProjectConnection
import org.gradle.tooling.ResultHandler
import org.gradle.tooling.events.ProgressEvent
import org.gradle.tooling.events.ProgressListener
import org.slf4j.LoggerFactory
import org.testeditor.dsl.common.ui.utils.ProjectUtils
import org.testeditor.tcl.dsl.ui.testlaunch.Launcher

class TclLauncher implements Launcher {
	static val logger = LoggerFactory.getLogger(TclLauncher)
	static val GRADLE_TEST_RESULT_FOLDER = "build/test-results"
	static val MVN_TEST_RESULT_FOLDER = "target/surefire-reports"

	@Inject extension ProjectUtils

	def void showTestResult(File testResult) {
		JUnitCore.importTestRunSession(testResult)
	}

	private def String elementIdToFileName(String elementId) {
		'''TEST-«elementId».xml'''
	}

	override boolean launch(IStructuredSelection selection, IProject project, String elementId, String mode) {
		if (project.getFile("build.gradle").exists) {
			return launchGradleBasedTest(project, elementId)
		}
		if (project.getFile("pom.xml").exists) {
			return launchMavenBasedTest(selection, project, elementId)
		}
		logger.warn('gradle based launching test for tcl element "{}" failed, since "build.gradle" was not found.',
			elementId)
		logger.warn('maven based launching test for tcl element "{}" failed, since "pom.xml" was not found.', elementId)
		return false
	}

	private def boolean launchGradleBasedTest(IProject project, String elementId) {
		logger.info("Trying to launch gradle test execution")
		val testResultFile = project.createOrGetDeepFolder(TclLauncher.GRADLE_TEST_RESULT_FOLDER).getFile(
			elementId.elementIdToFileName).location.toFile
		val GradleConnector connector = GradleConnector.newConnector
		var ProjectConnection connection = null
		val projectFolder = project.location.makeAbsolute.toFile
		try {
			connection = connector.forProjectDirectory(projectFolder).connect();
			// val BuildEnvironment environment = connection.model(BuildEnvironment).get();
			connection.newBuild.addProgressListener(new ProgressListener {

				override statusChanged(ProgressEvent event) {
					logger.info(event.displayName)
				}

			}) // .forTasks("test") // does not work, see issue below
			.withArguments("test", "--tests", elementId) // https://issues.gradle.org/browse/GRADLE-2972
			// .setStandardOutput(System.out) // alternatively get a separate console output stream (see http://wiki.eclipse.org/FAQ_How_do_I_write_to_the_console_from_a_plug-in%3F)
			.run(
				new ResultHandler {

					override onComplete(Object obj) {
						testResultFile.showTestResult
					}

					override onFailure(GradleConnectionException exception) {
						logger.
							error('''Caught error during gradle task "test" of element "«elementId»": «exception.toString»''',
								exception)
						testResultFile.showTestResult
					}

				})
		} finally {
			if (connection != null) {
				connection.close
			}
		}
		return true
	}

	private def boolean launchMavenBasedTest(IStructuredSelection selection, IProject project, String elementId) {
		logger.info("Trying to launch maven test execution")

		val job = new Job("Execute test " + elementId) {

			override protected run(IProgressMonitor monitor) {
				val mvnExec = new MavenExecutor

				val result = mvnExec.executeInNewJvm("integration-test", project.location.toOSString,
					"test=" + elementId)
				val testResultFile = project.createOrGetDeepFolder(TclLauncher.MVN_TEST_RESULT_FOLDER).getFile(
					elementId.elementIdToFileName).location.toFile
				if (result == 0) {
					testResultFile.showTestResult
				} else {
					logger.error('''Error during maven task "test" of element "«elementId»"''')
					// create error file?
				}
				return Status.OK_STATUS
			}

		}
		job.schedule
		return true
	}

}
