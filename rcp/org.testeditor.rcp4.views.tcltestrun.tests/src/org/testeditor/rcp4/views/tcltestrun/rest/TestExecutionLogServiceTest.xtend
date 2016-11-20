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

import com.google.gson.Gson
import java.io.StringReader
import java.nio.file.Files
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.dsl.common.testing.AbstractTest
import org.testeditor.rcp4.views.tcltestrun.LogLocationHelper
import org.testeditor.rcp4.views.tcltestrun.model.TestExecutionLog
import org.testeditor.rcp4.views.tcltestrun.model.TestExecutionLogList
import org.testeditor.rcp4.views.tcltestrun.model.TestExecutionManager

import static org.mockito.Mockito.*
import java.io.File
import org.testeditor.rcp4.views.tcltestrun.model.TestRunUtility

class TestExecutionLogServiceTest extends AbstractTest {

	@Mock
	LogLocationHelper logLocationHelper

	@InjectMocks
	TestExecutionManager executionManager

	@InjectMocks
	TestExecutionLogService testExecLogService

	@Rule
	public TemporaryFolder tempFolder = new TemporaryFolder

	@Test
	def void testGetTestLogExeutionList() {
		// given
		val executionManagerMock = mock(TestExecutionManager)
		testExecLogService.testExecutionManager = executionManagerMock
		val teLog1 = new TestExecutionLog
		teLog1.name = "17.10.16 08:18"
		teLog1.logDir = new File(new File("testrun-2016.10.17-08:18"),"testrun.log")
		val teLog2 = new TestExecutionLog
		teLog2.logDir = new File(new File("testrun-2016.10.17-21:30"),"testrun.log")
		teLog2.name = "17.10.16 21:30"
		when(executionManagerMock.testExecutionLogs).thenReturn(new TestExecutionLogList(#[teLog1, teLog2]))

		// when
		val listString = testExecLogService.testLogExeutionsList.entity as String
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
		testExecLogService.testExecutionManager = executionManager
		val teLog = new TestExecutionLog
		val baseDir = tempFolder.newFolder("testrun-2016.11.16-22:24")
		teLog.logDir = new File(baseDir,"testrun.log")
		Files.write(teLog.getLogDir.toPath, "Log content".bytes)
		teLog.logDir = new File(baseDir,"testSummary.xml")
		Files.write(teLog.getLogDir.toPath, TestRunUtility.testResult.bytes)
		when(logLocationHelper.logLocation).thenReturn(tempFolder.root)

		// when
		val listString = testExecLogService.testLogExeutionsList.entity as String
		val gson = new Gson
		val json = gson.fromJson(new StringReader(listString), TestExecutionLogList)
		val links = json.entries.get(0).links
		val logString = testExecLogService.getTestLogExeutionContent("testrun-2016.11.16-22:24").entity as String
		val log = gson.fromJson(new StringReader(logString), TestExecutionLog)

		// then
		assertEquals(links.length, 3)
		assertEquals(links.get(0).href,
			TestExecutionLogService.SERVICE_PATH + "/testrun-2016.11.16-22:24/fullLogs")
		assertEquals(links.get(1).href,
			TestExecutionLogService.SERVICE_PATH + "/testrun-2016.11.16-22:24/logGroups")
		log.content.assertEquals("Log content")
	}

	@Test
	def void testGetTestLogExeutionTestStepTree() {
		// given
		testExecLogService.testExecutionManager = executionManager
		val teLog = new TestExecutionLog
		val baseDir = tempFolder.newFolder("testrun-2016.11.16-22:24")
		teLog.logDir = new File(baseDir,"testrun.log")
		val logString = '''
			[INFO] --- xtend-maven-plugin:2.10.0:testCompile (default) @ org.testeditor.rcp4.uatests ---
			18:49:10 INFO  [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Test specification] * Given
			18:49:10 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Component] TestEditorServices		
		'''
		Files.write(teLog.logDir.toPath, logString.bytes)
		teLog.logDir = new File(baseDir,"testSummary.xml")
		Files.write(teLog.getLogDir.toPath, TestRunUtility.testResult.bytes)
		when(logLocationHelper.logLocation).thenReturn(tempFolder.root)
		
		// when
		val response = testExecLogService.getTestLogExeutionTestStepTree("testrun-2016.11.16-22:24")
		val gson = new Gson
		val log = gson.fromJson(new StringReader(response.entity as String), TestExecutionLog)
		
		// then
		log.logGroups.assertNotNull
		log.logGroups.assertNotEmpty
		log.logGroups.assertSize(2)
	}
	
}
