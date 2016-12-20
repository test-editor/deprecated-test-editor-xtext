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
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.dsl.common.testing.AbstractTest
import org.testeditor.rcp4.views.tcltestrun.LogLocationHelper

import static org.mockito.Mockito.*

class TestExecutionManagerTest extends AbstractTest {

	@Mock
	LogLocationHelper logLocationHelper

	@InjectMocks
	TestExecutionManager testExecutionManager

	@Rule
	public TemporaryFolder tempFolder = new TemporaryFolder

	@Test
	def void testGetTestExecutionLogs() {
		// given
		val dir = tempFolder.root
		Files.createFile(new File(dir, "foo.bar").toPath)
		val runDir1 = Files.createDirectory(new File(dir, "testrun-2016.11.16-22.24").toPath)
		val runDir2 = Files.createDirectory(new File(dir, "testrun-2016.11.16-23.24").toPath)
		Files.write(Files.createFile(new File(runDir1.toFile, "testSummary.xml").toPath),
			TestRunUtility.testResult.bytes)
		Files.write(Files.createFile(new File(runDir2.toFile, "testSummary.xml").toPath),
			TestRunUtility.testResult.bytes)
		when(logLocationHelper.logLocation).thenReturn(dir)

		// when
		val list = testExecutionManager.testExecutionLogs.entries.sortBy[it.name].reverse

		// then
		list.assertSize(2)
		list.get(0).getLogDir.parentFile.name.assertEquals("testrun-2016.11.16-23.24")
		list.get(0).name.assertEquals("16.11.16 23:24")
		list.get(1).getLogDir.parentFile.name.assertEquals("testrun-2016.11.16-22.24")
		list.get(1).name.assertEquals("16.11.16 22:24")
	}

	@Test
	def void testReadTestStatistic() {
		// given
		val testSummaryFile = new File(tempFolder.root, "testSummary.xml")
		Files.write(testSummaryFile.toPath, TestRunUtility.testResult.bytes)

		// when
		val statistic = testExecutionManager.readTestStatistic(testSummaryFile.parentFile)

		// then
		assertEquals(statistic.errors, 0)
		assertEquals(statistic.failures, 0)
		assertEquals(statistic.tests, 1)
	}
	
	@Test
	def void testDeleteTestRun() {
		// given
		val dir = tempFolder.root
		Files.createDirectory(new File(dir, "testrun-2016.11.16-22.24").toPath)
		Files.createDirectory(new File(dir, "testrun-2016.11.16-23.24").toPath)
		when(logLocationHelper.logLocation).thenReturn(dir)

		// when	
		testExecutionManager.delete("testrun-2016.11.16-23.24")
		
		// then	
		val testRuns = dir.list
		testRuns.assertSingleElement.assertEquals("testrun-2016.11.16-22.24")
	}
	
}
