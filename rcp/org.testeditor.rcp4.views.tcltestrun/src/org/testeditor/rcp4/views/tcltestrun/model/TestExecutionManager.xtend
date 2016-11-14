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
import java.util.Date
import java.util.List
import javax.inject.Inject
import org.testeditor.rcp4.views.tcltestrun.StateLocationHelper
import org.eclipse.e4.core.di.annotations.Creatable
import java.text.SimpleDateFormat
import org.slf4j.LoggerFactory

@Creatable
class TestExecutionManager {

	static val logger = LoggerFactory.getLogger(TestExecutionManager)

	@Inject StateLocationHelper stateLocationHelper

	def TestExecutionLog createTestExecutionLog(List<String> testNames) {
		return new TestExecutionLog => [
			testCases = testNames.map[new TestCaseExecution(it)]
			executionDate = new Date
		]
	}

	def OutputStream createOutputStreamFor(TestExecutionLog execLog) {
		val location = stateLocationHelper.stateLocation
		val newLog = new File(location, "te-" + execLog.executionDate.time + ".log")
		logger.info("Create new test execution log file {}.", newLog.absolutePath)
		return new FileOutputStream(newLog)
	}

	def TestExecutionLogList getTestExecutionLogs() {
		val location = stateLocationHelper.stateLocation
		val logs = location.list.filter[it.matches('te-\\d+\\.log')]
		return new TestExecutionLogList(logs.map [
			val log = new TestExecutionLog
			log.name = testExecutionLogName
			log.logFile = new File(location, it)
			return log
		].sortBy[name])
	}

	def private String getTestExecutionLogName(String teLogFileName) {
		val date = new Date(Long.parseLong(teLogFileName.substring(3, teLogFileName.lastIndexOf("."))))
		val sdf = new SimpleDateFormat("dd.MM.yy HH:mm")
		return sdf.format(date)
	}

}
