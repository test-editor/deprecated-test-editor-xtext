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
package org.testeditor.rcp4.views.tcltestrun.model

import java.io.File
import java.nio.file.Files
import java.text.SimpleDateFormat
import java.util.Date
import java.util.List
import javax.inject.Inject
import org.eclipse.e4.core.di.annotations.Creatable
import org.slf4j.LoggerFactory
import org.testeditor.rcp4.views.tcltestrun.LogLocationHelper

@Creatable
class TestExecutionManager {

	static val logger = LoggerFactory.getLogger(TestExecutionManager)
	static val String TIMESTAMP_FILE_PATTERN = "yyyy.MM.dd-HH:mm"

	@Inject LogLocationHelper logLocationHelper
	

	def TestExecutionLog createTestExecutionLog(List<String> testNames) {
		return new TestExecutionLog => [
			testCases = testNames.map[new TestCaseExecution(it)]
			executionDate = new Date
		]
	}

	def File createTestlogDirectoryFor(TestExecutionLog execLog) {
		val location = logLocationHelper.logLocation
		val sdf = new SimpleDateFormat(TIMESTAMP_FILE_PATTERN)
		val newLog = new File(location, "testrun-" + sdf.format(execLog.executionDate))
		if (newLog.mkdir) {
			logger.info("Create new test execution log file {}.", newLog.absolutePath)
		}
		return newLog
	}

	def TestExecutionLogList getTestExecutionLogs() {
		val location = logLocationHelper.logLocation
		val logs = location.list.filter[it.startsWith('testrun-')]
		return new TestExecutionLogList(logs.map [
			createTestExecutionLog
		].sortBy[-getLogDir.lastModified])
	}

	def TestExecutionLog gettestExecutionLogFor(String fileName) {
		val location = logLocationHelper.logLocation
		val log = location.list.filter[it.matches(fileName)]
		if (log.head != null) {
			val result = log.head.createTestExecutionLog
			result.content = Files.readAllLines(result.getLogDir.toPath).join
			return result
		}
		return null

	}

	def private TestExecutionLog createTestExecutionLog(String teLogFileName) {
		val log = new TestExecutionLog
		val location = logLocationHelper.logLocation
		log.name = teLogFileName.testExecutionLogName
		log.logDir = new File(new File(location, teLogFileName), "testrun.log")
		return log
	}

	def private String getTestExecutionLogName(String teLogFileName) {
		val sdfReader = new SimpleDateFormat(TIMESTAMP_FILE_PATTERN)
		val date = sdfReader.parse(teLogFileName.substring(8))
		val sdf = new SimpleDateFormat("dd.MM.yy HH:mm")
		return sdf.format(date)
	}

}
