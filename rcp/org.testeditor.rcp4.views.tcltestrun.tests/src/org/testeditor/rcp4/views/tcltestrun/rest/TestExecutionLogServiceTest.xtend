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

import java.io.StringReader
import javax.json.Json
import org.junit.Test
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.dsl.common.testing.AbstractTest
import org.testeditor.rcp4.views.tcltestrun.model.TestExecutionManager

import static org.mockito.Mockito.*
import org.testeditor.rcp4.views.tcltestrun.model.TestExecutionLog
import java.io.File
import org.junit.Rule
import org.junit.rules.TemporaryFolder
import java.nio.file.Files

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
		teLog1.testExecutionName = "17.10.16 08:18"
		teLog1.logFile = new File("te-1476685123287.log")
		val teLog2 = new TestExecutionLog
		teLog2.testExecutionName = "17.10.16 21:30"
		teLog2.logFile = new File("te-1476732656343.log")
		when(executionManager.testExecutionLogs).thenReturn(#[teLog1, teLog2])

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
		teLog.testExecutionName = "17.10.16 08:18"
		teLog.logFile = tempFolder.newFile("te-1476685123287.log")
		Files.write(teLog.logFile.toPath, "Log content".bytes)
		when(executionManager.testExecutionLogs).thenReturn(#[teLog])

		// when
		val listString = testExecLogService.testLogExeutionsList.entity as String
		val json = Json.createReader(new StringReader(listString)).readObject
		val links = json.getJsonArray("entries").getJsonObject(0).getJsonArray("links")
		val logString = testExecLogService.getTestLogExeutionContent("te-1476685123287.log").entity as String
		val log = Json.createReader(new StringReader(logString)).readObject

		// then
		assertEquals(links.length, 3)
		assertEquals(links.getJsonObject(0).getString("href"), "/testexeclogs/te-1476685123287.log/fulllogs")
		assertEquals(links.getJsonObject(1).getString("href"), "/testexeclogs/te-1476685123287.log/testSteps")
		log.getString("content").assertEquals("Log content")
	}

	@Test
	def void testComplexLog() {
		// given
		val log = '''
			[INFO] --- xtend-maven-plugin:2.10.0:testCompile (default) @ org.testeditor.rcp4.uatests ---
			18:49:10 INFO  [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Test specification] * Given
			18:49:10 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Component] TestEditorServices
			18:49:10 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [test step] - Click on <NextButton>
			18:49:12 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [test step] - Click on <FinishButton>
			18:49:10 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Component] TestEditorWizard
			18:49:10 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [test step] - Type "foo" into field <name>
			18:49:12 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [test step] - Click on <FinishButton>
		 '''

		// when
		val json = testExecLogService.createLogGroupJsonArray(log.split("\n")).build

		// then	 
		json.getJsonArray("logGroups").forEach[println(it)]
		assertEquals(json.getJsonArray("logGroups").size, 2)
		assertEquals(json.getJsonArray("logGroups").getJsonObject(0).getString("type"), "System")
		val spec = json.getJsonArray("logGroups").getJsonObject(1)
		assertEquals(spec.getString("type"), "Test specification")
		assertEquals(spec.getJsonArray("childs").size, 2)
		val componentService = spec.getJsonArray("childs").getJsonObject(0)
		assertEquals(componentService.getString("name"), "[Component] TestEditorServices")
		val nextButton = componentService.getJsonArray("childs").getJsonObject(0)
		println(nextButton)
		assertEquals(nextButton.getString("name"), "[test step] - Click on <NextButton>")
		val finishButton1 = componentService.getJsonArray("childs").getJsonObject(1)
		assertEquals(finishButton1.getString("name"), "[test step] - Click on <FinishButton>")

		val componentWizard = spec.getJsonArray("childs").getJsonObject(1)
		assertEquals(componentWizard.getString("name"), "[Component] TestEditorWizard")
		val field = componentWizard.getJsonArray("childs").getJsonObject(0)
		assertEquals(field.getString("name"), "[test step] - Type \"foo\" into field <name>")
		val finishButton2 = componentWizard.getJsonArray("childs").getJsonObject(1)
		assertEquals(finishButton2.getString("name"), "[test step] - Click on <FinishButton>")
	}

}
