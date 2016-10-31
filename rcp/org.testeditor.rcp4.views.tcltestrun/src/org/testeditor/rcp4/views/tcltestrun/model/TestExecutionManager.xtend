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

@Creatable
class TestExecutionManager {

	@Inject StateLocationHelper stateLocationHelper

	def TestExecutionLog createTestExecutionLog(List<String> testNames) {
		val result = new TestExecutionLog
		result.testCases = testNames.map[new TestCaseExecution(it)]
		result.executionDate = new Date
		return result
	}

	def OutputStream createOutputStreamFor(TestExecutionLog execLog) {
		val location = stateLocationHelper.stateLocation
		return new FileOutputStream(new File(location, "te-" + execLog.executionDate.time + ".log"))
	}

	def List<TestExecutionLog> getTestExecutionLogs() {
		val location = stateLocationHelper.stateLocation
		val logs = location.list.filter[it.matches('te-\\d+\\.log')]
		return logs.map[
			val log = new TestExecutionLog
			log.testExecutionName = testExecutionLogName
			log.logFile = new File(location,it)
			return log
		].toList
	}

	def private String getTestExecutionLogName(String teLogFileName) {
		val date = new Date(Long.parseLong(teLogFileName.substring(3,teLogFileName.lastIndexOf("."))))
		val sdf = new SimpleDateFormat("dd.MM.yy HH:mm")
		return sdf.format(date)
	}

}
