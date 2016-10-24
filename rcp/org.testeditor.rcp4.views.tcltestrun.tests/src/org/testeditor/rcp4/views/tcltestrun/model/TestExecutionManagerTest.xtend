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

import org.testeditor.dsl.common.testing.AbstractTest
import org.mockito.Mock
import org.mockito.InjectMocks
import org.testeditor.rcp4.views.tcltestrun.StateLocationHelper
import org.junit.Test

import static org.mockito.Mockito.*
import java.io.File

class TestExecutionManagerTest extends AbstractTest {

	@Mock
	StateLocationHelper stateLocationHelper
	
	@InjectMocks
	TestExecutionManager testExecutionManager
	
	@Test
	def void testGetTestExecutionLogs() {
		// given
		val dir = mock(File)
		when(dir.list).thenReturn(#["foo.bar","te-1476685123287.log", "te-1476732656343.log"])
		when(stateLocationHelper.stateLocation).thenReturn(dir)
		
		// when
		val list = testExecutionManager.testExecutionLogs
		
		// then
		assertEquals(list.size,2)
		assertEquals(list.get(0),"te-1476685123287.log")
		assertEquals(list.get(1),"te-1476732656343.log")
	}
	
}