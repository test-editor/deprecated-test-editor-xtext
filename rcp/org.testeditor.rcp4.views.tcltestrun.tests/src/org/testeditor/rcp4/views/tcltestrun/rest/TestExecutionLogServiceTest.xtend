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
package org.testeditor.rcp4.views.tcltestrun.rest

import java.io.File
import java.io.StringReader
import java.nio.file.Files
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.dsl.common.testing.AbstractTest
import org.testeditor.rcp4.views.tcltestrun.model.TestExecutionLog
import org.testeditor.rcp4.views.tcltestrun.model.TestExecutionLogList
import org.testeditor.rcp4.views.tcltestrun.model.TestExecutionManager

import static org.mockito.Mockito.*

class TestExecutionLogServiceTest extends AbstractTest {

	@Mock
	TestExecutionManager executionManager

	@InjectMocks
	TestExecutionLogService testExecLogService

	@Rule
	public TemporaryFolder tempFolder = new TemporaryFolder();

	@Test
	def void testGetTestLogExeutionList() {
		// given
		testExecLogService.testExecutionManager = executionManager
		val teLog1 = new TestExecutionLog
		teLog1.name = "17.10.16 08:18"
		teLog1.logFile = new File("te-1476685123287.log")
		val teLog2 = new TestExecutionLog
		teLog2.name = "17.10.16 21:30"
		teLog2.logFile = new File("te-1476732656343.log")
		when(executionManager.testExecutionLogs).thenReturn(new TestExecutionLogList(#[teLog1, teLog2]))

		// when
		val listString = testExecLogService.testLogExeutionsList.entity as String
		val json = Json.createReader(new StringReader(listString)).readObject

		// then
		assertEquals(json.getJsonArray("entries").length, 2)
		assertEquals(json.getJsonArray("entries").getJsonObject(0).getString("name"), "17.10.16 08:18")
		assertEquals(json.getJsonArray("entries").getJsonObject(1).getString("name"), "17.10.16 21:30")
	}

	@Test
	def void testFullLogsFromListItem() {
		// given
		testExecLogService.testExecutionManager = executionManager
		val teLog = new TestExecutionLog
		teLog.name = "17.10.16 08:18"
		teLog.logFile = tempFolder.newFile("te-1476685123287.log")
		Files.write(teLog.logFile.toPath, "Log content".bytes)
		when(executionManager.testExecutionLogs).thenReturn(new TestExecutionLogList(#[teLog]))

		// when
		val listString = testExecLogService.testLogExeutionsList.entity as String
		val json = Json.createReader(new StringReader(listString)).readObject
		val links = json.getJsonArray("entries").getJsonObject(0).getJsonArray("links")
		val logString = testExecLogService.getTestLogExeutionContent("te-1476685123287.log").entity as String
		val log = Json.createReader(new StringReader(logString)).readObject

		// then
		assertEquals(links.length, 3)
		assertEquals(links.getJsonObject(0).getString("href"),
			TestExecutionLogService.SERVICE_PATH + "/te-1476685123287.log/fullLogs")
		assertEquals(links.getJsonObject(1).getString("href"),
			TestExecutionLogService.SERVICE_PATH + "/te-1476685123287.log/logGroups")
		log.getString("content").assertEquals("Log content")
	}


}
