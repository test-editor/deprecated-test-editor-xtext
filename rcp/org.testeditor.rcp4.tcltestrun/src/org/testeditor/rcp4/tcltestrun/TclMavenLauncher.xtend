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

import java.io.ByteArrayOutputStream
import java.io.File
import java.io.OutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.List
import java.util.Map
import java.util.Scanner
import javax.inject.Inject
import javax.swing.JOptionPane
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.IStatus
import org.eclipse.core.runtime.NullProgressMonitor
import org.slf4j.LoggerFactory
import org.testeditor.dsl.common.ui.utils.ProjectUtils
import org.testeditor.dsl.common.util.MavenExecutor
import org.gradle.internal.impldep.org.apache.commons.lang.SystemUtils

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
		if(!checkMavenVersion(project, monitor)) {
			return new LaunchResult(null, false, null)			
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

	def checkMavenVersion(IProject project, IProgressMonitor monitor) {
		val versionOut = new ByteArrayOutputStream()
		val infoResult = mavenExecutor.executeInNewJvm("-version", project.location.toOSString, "", monitor, versionOut)
		if (infoResult != IStatus.OK) {
			logger.error("Error during determine maven version")
			return false
		}
		val versionInfoScanner = new Scanner(versionOut.toString).useDelimiter("\n");
		
		while (versionInfoScanner.hasNext) {
			val line = versionInfoScanner.next()
			if (line.startsWith("Apache Maven ")) {
				var version = line.substring("Apache Maven ".length)
				logger.info("Maven Version: {} ", version)
				if (!version.startsWith("3.2.5")) {
					version = version.substring(0, version.indexOf(' '))
					if (JOptionPane.showConfirmDialog(null, "Wrong maven-version '" + version +
						"' in MAVEN_HOME. For best test execution please use 3.2.5. Start test anyway?") != 0) {
						return false
					}
				}
			} else if (line.indexOf(':') != -1) {
				logger.info("Maven Property '{}' : '{}'", line.substring(0, line.indexOf(':')).trim(),
					line.substring(line.indexOf(':') + 1).trim())
			}
		}
		return true
	}

	def Iterable<String> getProfiles(IProject project) {
		mavenExecutor.executeInNewJvm("help:all-profiles", project.location.toOSString, '''output=«PROFILE_TXT_PATH»''',
			new NullProgressMonitor, System.out)
		val file = new File('''«project.location.toOSString»/«PROFILE_TXT_PATH»''')
		val profileOutput = Files.readAllLines(file.toPath, StandardCharsets.UTF_8)
		return profileOutput.filter[contains("Profile Id:")].map [
			substring(indexOf("Id:") + 3, indexOf("(")).trim
		].toSet
	}

}
