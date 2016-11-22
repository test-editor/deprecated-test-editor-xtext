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
import java.io.FileOutputStream
import java.io.OutputStream
import java.text.SimpleDateFormat
import java.util.Date
import java.util.List
import javax.inject.Inject
import org.eclipse.e4.core.di.annotations.Creatable
import org.slf4j.LoggerFactory
import org.testeditor.rcp4.views.tcltestrun.LogLocationHelper
import java.nio.file.Files

@Creatable
class TestExecutionManager {

	static val logger = LoggerFactory.getLogger(TestExecutionManager)
	
	val fileDateFormat = new SimpleDateFormat("yyyy.MM.dd-HH.mm")

	@Inject LogLocationHelper logLocationHelper

	def TestExecutionLog createTestExecutionLog(List<String> testNames) {
		return new TestExecutionLog => [
			testCases = testNames.map[new TestCaseExecution(it)]
			executionDate = new Date
		]
	}

	def OutputStream createOutputStreamFor(TestExecutionLog execLog) {
		val location = logLocationHelper.logLocation
		val newLog = new File(location, "te-" + fileDateFormat.format(execLog.executionDate) + ".log")
		logger.info("Create new test execution log file {}.", newLog.absolutePath)
		return new FileOutputStream(newLog)
	}

	def TestExecutionLogList getTestExecutionLogs() {
		val location = logLocationHelper.logLocation
		val logs = location.list.filter[it.startsWith('te-') && it.endsWith('.log')]
		return new TestExecutionLogList(logs.map [
			createTestExecutionLog
		].sortBy[-logFile.lastModified])
	}

	def TestExecutionLog getTestExecutionLogFor(String fileName) {
		val location = logLocationHelper.logLocation
		val log = location.list.findFirst[matches(fileName)]
		if (log !== null) {
			val result = log.createTestExecutionLog
			result.content = Files.readAllLines(result.logFile.toPath).join
			return result
		}
		return null

	}

	def private TestExecutionLog createTestExecutionLog(String teLogFileName) {
		val log = new TestExecutionLog
		val location = logLocationHelper.logLocation
		log.name = teLogFileName.testExecutionLogName
		log.logFile = new File(location, teLogFileName)
		return log
	}

	def private String getTestExecutionLogName(String teLogFileName) {
		val date = fileDateFormat.parse(teLogFileName.substring(3, teLogFileName.lastIndexOf(".")))
		val sdf = new SimpleDateFormat("dd.MM.yy HH:mm")
		return sdf.format(date)
	}

}
