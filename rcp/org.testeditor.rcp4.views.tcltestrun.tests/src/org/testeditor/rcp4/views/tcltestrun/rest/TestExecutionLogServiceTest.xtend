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

class TestExecutionLogServiceTest extends AbstractTest {

	@Mock
	TestExecutionManager executionManager

	@InjectMocks
	TestExecutionLogService testExecLogService

	@Test
	def void testGetTestLogExeutionList() {
		// given
		testExecLogService.testExecutionManager = executionManager
		when(executionManager.testExecutionLogs).thenReturn(#["te-1476685123287.log", "te-1476732656343.log"])

		// when
		val listString = testExecLogService.testLogExeutionList.entity as String
		val json = Json.createReader(new StringReader(listString)).readObject

		// then
		assertEquals(json.getJsonArray("entries").length, 2)
		assertEquals(json.getJsonArray("entries").getJsonObject(0).getString("name"),"17.10.16 08:18")
		assertEquals(json.getJsonArray("entries").getJsonObject(1).getString("name"),"17.10.16 21:30")
	}
}
