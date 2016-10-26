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

@Creatable
class TestExecutionManager {

	@Inject StateLocationHelper stateLocationHelper

	def TestExecutionLog createTestExecutionLog(List<String> strings) {
		val result = new TestExecutionLog
		result.testSteps = strings
		result.executionDate = new Date
		return result
	}

	def OutputStream createOutputStreamFor(TestExecutionLog execLog) {
		val location = stateLocationHelper.stateLocation
		return new FileOutputStream(new File(location, "te-" + execLog.executionDate.time + ".log"))
	}

	def List<String> getTestExecutionLogs() {
		val location = stateLocationHelper.stateLocation
		return location.list().filter[it.matches('te-\\d+\\.log')].toList
	}

}
