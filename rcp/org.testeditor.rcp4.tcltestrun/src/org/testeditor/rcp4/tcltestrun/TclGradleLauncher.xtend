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
import java.io.OutputStream
import java.util.List
import java.util.Map
import javax.inject.Inject
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor
import org.gradle.tooling.GradleConnectionException
import org.gradle.tooling.ResultHandler
import org.slf4j.LoggerFactory
import org.testeditor.dsl.common.ui.gradle.GradleHelper
import org.testeditor.dsl.common.ui.utils.ProjectUtils

public class TclGradleLauncher implements TclLauncher {

	static val GRADLE_TEST_RESULT_FOLDER = "build/test-results"
	static val logger = LoggerFactory.getLogger(TclGradleLauncher)

	@Inject extension ProjectUtils
	@Inject GradleHelper gradleHelper

	override launchTest(List<String> testCases, IProject project, IProgressMonitor monitor, OutputStream out, Map<String, Object> options) {
		val testCase = testCases.head // currently only works with a single test case
		monitor.beginTask('''Running gradle test for testCase='«testCase»'.''', IProgressMonitor.UNKNOWN)

		val testResultFolder = project.createOrGetDeepFolder(GRADLE_TEST_RESULT_FOLDER).location.toFile
		val resultHandler = new TclGradleResultHandler(testResultFolder)
		gradleHelper.run(project.location.toFile, resultHandler) [
			withArguments("clean", "test", "--tests", testCase) // https://issues.gradle.org/browse/GRADLE-2972
			standardOutput = out
			standardError = out
		]
		return resultHandler.result
	}

	@FinalFieldsConstructor
	private static class TclGradleResultHandler implements ResultHandler<Object> {

		val File testResultFolder
		LaunchResult result

		override onComplete(Object arg0) {
			result = new LaunchResult(testResultFolder)
		}

		override onFailure(GradleConnectionException e) {
			logger.error('''Caught error during gradle test execution''', e)
			result = new LaunchResult(testResultFolder, false, e)
		}

	}

}
