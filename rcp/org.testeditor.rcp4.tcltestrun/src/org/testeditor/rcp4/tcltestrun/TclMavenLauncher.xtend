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
import java.nio.file.Files
import java.util.Map
import javax.inject.Inject
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jface.viewers.IStructuredSelection
import org.slf4j.LoggerFactory
import org.testeditor.dsl.common.ui.utils.ProjectUtils
import java.nio.charset.StandardCharsets
import org.eclipse.ui.PlatformUI
import org.eclipse.e4.core.contexts.IEclipseContext
import org.osgi.service.prefs.PreferencesService
import org.testeditor.dsl.common.ui.utils.Constants

public class TclMavenLauncher implements TclLauncher {

	public static val String PROFILE = "profile"

	static val logger = LoggerFactory.getLogger(TclMavenLauncher)
	static val MVN_TEST_RESULT_FOLDER = "target/surefire-reports"
	static val PROFILE_TXT_PATH = "target/profiles.txt"

	@Inject extension ProjectUtils
	@Inject MavenExecutor mavenExecutor

	private def String elementIdToFileName(String elementId) {
		'''TEST-«elementId».xml'''
	}

	override launchTest(IStructuredSelection selection, IProject project, String elementId, IProgressMonitor monitor,
		Map<String, Object> options) {
		val parameters = if (options.containsKey(
				PROFILE)) {
				"clean generate-test-sources org.testeditor:testeditor-maven-plugin:testEnvUp org.testeditor:testeditor-maven-plugin:testExec -P" +
					options.get(PROFILE)
			} else {
				"clean integration-test"
			}
		val result = mavenExecutor.executeInNewJvm(parameters, project.location.toOSString, "test=" + elementId)
		val testResultFile = project.createOrGetDeepFolder(MVN_TEST_RESULT_FOLDER).getFile(
			elementId.elementIdToFileName).location.toFile
		if (result != 0) {
			logger.error('''Error during maven build using parameters='«parameters»' and element='«elementId»' ''')
		}
		return new LaunchResult(testResultFile, result, null)
	}
	
	def Iterable<String> getProfiles(IProject project) {
		mavenExecutor.executeInNewJvm("help:all-profiles", project.location.toOSString, '''output=«PROFILE_TXT_PATH»''')
		val file = new File('''«project.location.toOSString»/«PROFILE_TXT_PATH»''')
		val profileOutput = Files.readAllLines(file.toPath, StandardCharsets.UTF_8)
		return profileOutput.filter[contains("Profile Id:")].map[substring(indexOf("Id:") + 3, indexOf("(")).trim].toSet
	}

}
