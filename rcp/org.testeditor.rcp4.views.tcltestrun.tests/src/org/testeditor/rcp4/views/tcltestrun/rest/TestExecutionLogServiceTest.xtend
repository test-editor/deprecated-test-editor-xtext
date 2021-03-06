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
package org.testeditor.rcp4.views.tcltestrun.rest

import com.google.common.io.Files
import com.google.gson.Gson
import java.io.File
import java.io.StringReader
import java.nio.charset.StandardCharsets
import org.junit.Test
import org.mockito.InjectMocks
import org.testeditor.rcp4.views.tcltestrun.model.AbstractTestExecutionManagerTest
import org.testeditor.rcp4.views.tcltestrun.model.TestExecutionLog
import org.testeditor.rcp4.views.tcltestrun.model.TestExecutionLogList
import org.testeditor.rcp4.views.tcltestrun.model.TestExecutionManager

import static org.mockito.Mockito.*

class TestExecutionLogServiceTest extends AbstractTestExecutionManagerTest {

	@InjectMocks TestExecutionLogService testExecLogService

	@Test
	def void testGetTestLogExecutionList() {
		// given
		val executionManagerMock = mock(TestExecutionManager)
		testExecLogService.testExecutionManager = executionManagerMock
		val log1 = new TestExecutionLog => [
			name = "17.10.16 08:18"
			logDir = new File(new File("testrun-2016.10.17-08.18.22.567"), "testrun.log")
		]
		val log2 = new TestExecutionLog => [
			name = "17.10.16 21:30"
			logDir = new File(new File("testrun-2016.10.17-21.30.25.678"), "testrun.log")
		]
		when(executionManagerMock.testExecutionLogs).thenReturn(new TestExecutionLogList(#[log1, log2]))

		// when
		val listString = testExecLogService.testLogExecutionsList.entity as String
		val gson = new Gson
		val list = gson.fromJson(new StringReader(listString), TestExecutionLogList)

		// then
		assertEquals(list.entries.length, 2)
		assertEquals(list.entries.get(0).name, "17.10.16 08:18")
		assertEquals(list.entries.get(1).name, "17.10.16 21:30")
	}

	@Test
	def void testFullLogsFromListItem() {
		// given
		testExecLogService.testExecutionManager = testExecutionManager
		createTestLogServiceEnvironmentWithLogContent("Log Content")

		// when
		val listString = testExecLogService.testLogExecutionsList.entity as String
		val gson = new Gson
		val json = gson.fromJson(new StringReader(listString), TestExecutionLogList)
		val links = json.entries.get(0).links
		val logString = testExecLogService.getTestLogExecutionContent("testrun-2016.11.16-22.24.05.333").entity as String
		val log = gson.fromJson(new StringReader(logString), TestExecutionLog)

		// then
		assertEquals(links.length, 3)
		assertEquals(links.get(0).href, TestExecutionLogService.SERVICE_PATH + "/testrun-2016.11.16-22.24.05.333/fullLogs")
		assertEquals(links.get(1).href, TestExecutionLogService.SERVICE_PATH + "/testrun-2016.11.16-22.24.05.333/logGroups")
		log.content.assertEquals("Log Content")
	}

	@Test
	def void testGetTestLogExecutionTestStepTree() {
		// given
		testExecLogService.testExecutionManager = testExecutionManager
		val logString = '''
			[INFO] --- xtend-maven-plugin:2.10.0:testCompile (default) @ org.testeditor.rcp4.uatests ---
			18:49:10 INFO  [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Spec step] * Given
			18:49:10 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Component] ** TestEditorServices		
		'''
		createTestLogServiceEnvironmentWithLogContent(logString)

		// when
		val response = testExecLogService.getTestLogExecutionTestStepTree("testrun-2016.11.16-22.24.05.333")
		val gson = new Gson
		val log = gson.fromJson(new StringReader(response.entity as String), TestExecutionLog)

		// then
		log.logGroups.assertNotNull
		log.logGroups.assertSize(2)
	}

	private def void createTestLogServiceEnvironmentWithLogContent(String logContent) {
		val runSummary = createTestRunSummary("testrun-2016.11.16-22.24.05.333")
		val testLog = new File(runSummary.parentFile, "testrun.log")
		Files.write(logContent, testLog, StandardCharsets.UTF_8)
	}

}
