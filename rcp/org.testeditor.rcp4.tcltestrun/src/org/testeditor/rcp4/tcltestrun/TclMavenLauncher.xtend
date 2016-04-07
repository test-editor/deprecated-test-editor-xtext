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

import java.util.List
import java.util.Map
import javax.inject.Inject
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jface.viewers.IStructuredSelection
import org.slf4j.LoggerFactory
import org.testeditor.dsl.common.ui.utils.ProjectUtils

public class TclMavenLauncher implements TclLauncher {

	public static val String PROFILE="profile" 

	static val logger = LoggerFactory.getLogger(TclMavenLauncher)
	static val MVN_TEST_RESULT_FOLDER = "target/surefire-reports"

	@Inject extension ProjectUtils
	@Inject MavenExecutor mavenExecutor

	private def String elementIdToFileName(String elementId) {
		'''TEST-«elementId».xml'''
	}

	override launchTest(IStructuredSelection selection, IProject project, String elementId, IProgressMonitor monitor,
		Map<String, Object> options) {
		val goal = "integration-test"
		val result = mavenExecutor.executeInNewJvm(goal, project.location.toOSString, "test=" + elementId)
		val testResultFile = project.createOrGetDeepFolder(MVN_TEST_RESULT_FOLDER).getFile(
			elementId.elementIdToFileName).location.toFile
		if (result != 0) {
			logger.error('''Error during maven task='«goal»' of element='«elementId»' ''')
		}
		return #{RETURN_CODE -> result, EXPECTED_FILE -> testResultFile}
	}

	def List<String> getProfiles() {
		#["test"]
	}

}
