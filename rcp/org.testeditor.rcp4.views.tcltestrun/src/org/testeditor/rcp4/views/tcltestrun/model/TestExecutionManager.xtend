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
package org.testeditor.rcp4.views.tcltestrun.model

import java.io.File
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException
import java.util.List
import javax.inject.Inject
import javax.xml.parsers.DocumentBuilderFactory
import org.apache.commons.io.FileUtils
import org.eclipse.e4.core.di.annotations.Creatable
import org.slf4j.LoggerFactory
import org.testeditor.rcp4.views.tcltestrun.LogLocationHelper
import org.w3c.dom.NamedNodeMap

@Creatable
class TestExecutionManager {

	static val logger = LoggerFactory.getLogger(TestExecutionManager)

	static val String TEST_RUN_DIRECTORY_PREFIX = "testrun-"

	val fileDateFormat = DateTimeFormatter.ofPattern("yyyy.MM.dd-HH.mm.ss.SSS")
	val uiDateFormat = DateTimeFormatter.ofPattern("dd.MM.yy HH:mm")

	@Inject LogLocationHelper logLocationHelper

	def TestExecutionLog createTestExecutionLog(List<String> testNames) {
		return new TestExecutionLog => [
			testCases = testNames.map[new TestCaseExecution(it)]
			executionDate = LocalDateTime.now
		]
	}

	def File createTestlogDirectoryFor(TestExecutionLog execLog) {
		val location = logLocationHelper.logLocation
		val newLog = new File(location, TEST_RUN_DIRECTORY_PREFIX + fileDateFormat.format(execLog.executionDate))
		execLog.testRunTimestamp = newLog.name
		if (newLog.mkdir) {
			logger.info("Create new test execution log file {}.", newLog.absolutePath)
		}
		return newLog
	}

	def TestExecutionLogList getTestExecutionLogs() {
		val location = logLocationHelper.logLocation
		val logs = location.list.filter[it.startsWith(TEST_RUN_DIRECTORY_PREFIX)]
		return new TestExecutionLogList(logs.map [
			createTestExecutionLog
		].sortBy[getLogDir.lastModified].reverse)
	}

	def TestExecutionLog getTestExecutionLogFor(String fileName) {
		val location = logLocationHelper.logLocation
		val log = location.list.findFirst[matches(fileName)]
		if (log !== null) {
			val result = log.createTestExecutionLog
			result.content = org.testeditor.dsl.common.ide.util.FileUtils.readAllLines(result.logDir).join
			return result
		}
		return null

	}

	def File getScreenshotFor(String fileName, String testcasename, String screenshotPath) {
		val location = logLocationHelper.logLocation
		val log = location.listFiles.findFirst[it.name.matches(fileName)]
		if (log !== null) {
			return new File(log, '''screenshots«File.separator»«testcasename»«File.separator»«screenshotPath»''')
		}
		return null
	}

	def private TestExecutionLog createTestExecutionLog(String teLogFileName) {
		val log = new TestExecutionLog
		val location = logLocationHelper.logLocation
		val testRunBaseDir = new File(location, teLogFileName)
		log.testStatistic = readTestStatistic(testRunBaseDir)
		log.name = teLogFileName.testExecutionLogName
		log.logDir = new File(testRunBaseDir, "testrun.log")
		return log
	}

	def TestRunStatistic readTestStatistic(File parentDir) {
		val resultFile = new File(parentDir, "testSummary.xml")
		if (!resultFile.exists) {
			return new TestRunStatistic(0, 0, 0)
		}
		val docBuilder = DocumentBuilderFactory.newInstance.newDocumentBuilder
		val xml = docBuilder.parse(resultFile)
		val testStatistic = xml.firstChild.attributes
		return new TestRunStatistic(testStatistic.readIntFor("tests"), testStatistic.readIntFor("failures"),
			testStatistic.readIntFor("errors"))
	}

	def private int readIntFor(NamedNodeMap map, String name) {
		return Integer.parseInt(map.getNamedItem(name).nodeValue)
	}

	def private String getTestExecutionLogName(String teLogFileName) {
		logger.debug("Parsing log name from '{}'.", teLogFileName)
		try {
			val date = fileDateFormat.parse(teLogFileName.substring(8))
			return uiDateFormat.format(date)
		} catch (DateTimeParseException e) {
			logger.warn("Parsing log name failed, using '{}' instead.", teLogFileName)
			return teLogFileName
		}
	}

	def delete(String fileName) {
		val location = logLocationHelper.logLocation
		val log = location.listFiles.findFirst[it.name.matches(fileName)]
		if (log !== null) {
			FileUtils.deleteDirectory(log)
		}
	}

}
