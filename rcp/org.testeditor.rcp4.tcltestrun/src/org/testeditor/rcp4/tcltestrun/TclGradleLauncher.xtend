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

import java.util.Map
import java.util.concurrent.atomic.AtomicReference
import javax.inject.Inject
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.IProgressMonitor
import org.gradle.tooling.GradleConnectionException
import org.gradle.tooling.GradleConnector
import org.gradle.tooling.ProjectConnection
import org.gradle.tooling.ResultHandler
import org.gradle.tooling.events.OperationType
import org.slf4j.LoggerFactory
import org.testeditor.dsl.common.ui.utils.ProjectUtils

public class TclGradleLauncher implements TclLauncher {

	static val logger = LoggerFactory.getLogger(TclGradleLauncher)
	static val GRADLE_TEST_RESULT_FOLDER = "build/test-results"

	@Inject extension ProjectUtils

	private def String elementIdToFileName(String elementId) {
		'''TEST-«elementId».xml'''
	}

	override launchTest(String testCasesCommaList, IProject project, IProgressMonitor monitor,
		Map<String, Object> options) {
		val tests = testCasesCommaList.split(",")
		val elementId = tests.get(0)
		monitor.beginTask("Test execution: " + elementId, IProgressMonitor.UNKNOWN)
		val testResultFile = project.createOrGetDeepFolder(GRADLE_TEST_RESULT_FOLDER).getFile(
			elementId.elementIdToFileName).location.toFile.parentFile
		val GradleConnector connector = GradleConnector.newConnector
		var ProjectConnection connection = null
		val result = new AtomicReference<LaunchResult>
		try {
			val projectFolder = project.location.makeAbsolute.toFile
			val task = "test"
			connection = connector.forProjectDirectory(projectFolder).connect();
			// val BuildEnvironment environment = connection.model(BuildEnvironment).get();
			connection.newBuild =>
				[
					addProgressListener([event|logger.info("Gradle build event='{}'", event.displayName)],
						OperationType.values)
					// .forTasks("test") // does not work, see issue below
					withArguments("clean", task, "--tests", elementId) // https://issues.gradle.org/browse/GRADLE-2972
					// .setStandardOutput(System.out) // alternatively get a separate console output stream (see http://wiki.eclipse.org/FAQ_How_do_I_write_to_the_console_from_a_plug-in%3F)
					run(new ResultHandler {

						override onComplete(Object obj) {
							result.set(new LaunchResult(testResultFile, 0, null))
						}

						override onFailure(GradleConnectionException e) {
							logger.error('''Caught error during gradle task='«task»' of elementId='«elementId»' ''', e)
							result.set(new LaunchResult(testResultFile, 1, e))
						}

					})
				]
		} finally {
			if (connection != null) {
				connection.close
			}
		}
		return result.get
	}

}
