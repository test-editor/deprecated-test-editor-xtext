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
import java.nio.file.Files
import org.junit.Test

class TestExecutionManagerTest extends AbstractTestExecutionManagerTest {

	@Test
	def void testGetTestExecutionLogs() {
		// given
		val dir = tempFolder.root
		Files.createFile(new File(dir, "foo.bar").toPath)
		createTestRunSummary("testrun-2016.11.16-22.24.05.333")
		createTestRunSummary("testrun-2016.11.16-23.38.26.834")

		// when
		val logEntries = testExecutionManager.testExecutionLogs.entries

		// then
		logEntries.assertSize(2)
		logEntries.findFirst[name == "16.11.16 22:24"] => [
			logDir.parentFile.name.assertEquals("testrun-2016.11.16-22.24.05.333")
		]
		logEntries.findFirst[name == "16.11.16 23:38"] => [
			logDir.parentFile.name.assertEquals("testrun-2016.11.16-23.38.26.834")
		]
	}

	@Test
	def void testReadTestStatistic() {
		// given
		val testSummary = createTestRunSummary("testrun-dummy")

		// when
		val statistic = testExecutionManager.readTestStatistic(testSummary.parentFile)

		// then
		statistic => [
			tests.assertEquals(1)
			failures.assertEquals(0)
			errors.assertEquals(0)
		]
	}

	@Test
	def void testDeleteTestRun() {
		// given
		createTestRunSummary("testrun-2016.11.16-22.24.05.333")
		createTestRunSummary("testrun-2016.11.16-23.38.26.834")

		// when	
		testExecutionManager.delete("testrun-2016.11.16-23.38.26.834")

		// then	
		val testRuns = tempFolder.root.list
		testRuns.assertSingleElement.assertEquals("testrun-2016.11.16-22.24.05.333")
	}

}
