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
import java.time.LocalDateTime
import java.util.List
import org.eclipse.xtend.lib.annotations.Accessors

@Accessors
class TestExecutionLog {

	String name
	LocalDateTime executionDate
	List<TestCaseExecution> testCases
	String testRunTimestamp
	File logDir
	Link[] links
	String content
	TestLogGroup[] logGroups
	TestRunStatistic testStatistic
		
	def void setLogDir(File file) {
		logDir = file
		testRunTimestamp = logDir.parentFile.name
	}
		
}
