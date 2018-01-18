/*******************************************************************************
 * Copyright (c) 2012 - 2018 Signal Iduna Corporation and others.
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
import org.eclipse.core.runtime.IStatus
import org.eclipse.core.runtime.NullProgressMonitor
import org.slf4j.LoggerFactory
import org.testeditor.dsl.common.ide.util.FileUtils
import org.testeditor.dsl.common.ui.utils.ProjectUtils
import org.testeditor.dsl.common.util.MavenExecutor
import org.testeditor.dsl.common.util.MavenExecutor.MavenVersionValidity

public class TclMavenLauncher implements TclLauncher {

	public static val String PROFILE = "profile"

	static val logger = LoggerFactory.getLogger(TclMavenLauncher)
	static val MVN_TEST_RESULT_FOLDER = "target/surefire-reports"
	static val PROFILE_TXT_PATH = "target/profiles.txt"

	@Inject extension ProjectUtils
	@Inject MavenExecutor mavenExecutor

	override LaunchResult launchTest(List<String> testCases, IProject project, IProgressMonitor monitor,
		OutputStream out, Map<String, Object> options) {
		val parameters = if (options.containsKey(
				PROFILE)) {
				"clean generate-test-sources org.testeditor:testeditor-maven-plugin:testEnvUp org.testeditor:testeditor-maven-plugin:testExec -P" +
					options.get(PROFILE)
			} else {
				"clean integration-test"
			}

		var testSelectionArgument = ""
		if (testCases != null) {
			testSelectionArgument = "test=" + testCases.join(",")
		}

		val result = mavenExecutor.executeInNewJvm(parameters, project.location.toOSString, testSelectionArgument,
			monitor, out)
		val testResultFolder = project.createOrGetDeepFolder(MVN_TEST_RESULT_FOLDER).location.toFile
		if (result == IStatus.OK) {
			return new LaunchResult(testResultFolder)
		} else if (result == IStatus.CANCEL) {
			logger.info("User aborted execution of maven build.")
			// TODO there should be a different launch result here
			return new LaunchResult(testResultFolder, false, null)
		} else {
			logger.error("Error during maven build using parameters='{}' and element='«»', result='{}'.", parameters,
				testCases)
			return new LaunchResult(testResultFolder, false, null)
		}

	}

	def Iterable<String> getProfiles(IProject project) {
		mavenExecutor.executeInNewJvm("help:all-profiles", project.location.toOSString, '''output=«PROFILE_TXT_PATH»''',
			new NullProgressMonitor, System.out)
		val file = new File('''«project.location.toOSString»/«PROFILE_TXT_PATH»''')
		val profileOutput = FileUtils.readAllLines(file)
		return profileOutput.filter[contains("Profile Id:")].map [
			substring(indexOf("Id:") + 3, indexOf("(")).trim
		].toSet
	}
	
	def MavenVersionValidity getMavenVersionValidity(){
		return mavenExecutor.mavenVersionValidity
	}
	
}
