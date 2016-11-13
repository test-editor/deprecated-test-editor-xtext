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
import org.junit.Rule
import org.junit.rules.TemporaryFolder
import java.nio.file.Files

class TestExecutionManagerTest extends AbstractTest {

	@Mock
	StateLocationHelper stateLocationHelper
	
	@InjectMocks
	TestExecutionManager testExecutionManager
	
	@Rule 
	public TemporaryFolder tempFolder = new TemporaryFolder();
	
	@Test
	def void testGetTestExecutionLogs() {
		// given
		val dir = tempFolder.root
		Files.createFile(new File(dir,"foo.bar").toPath)
		Files.createFile(new File(dir,"te-1476685123287.log").toPath)
		Files.createFile(new File(dir,"te-1476732656343.log").toPath)
		when(stateLocationHelper.stateLocation).thenReturn(dir)
		
		// when
		val list = testExecutionManager.testExecutionLogs.entries
		
		// then
		list.assertSize(2)
		list.get(0).logFile.name.assertEquals("te-1476685123287.log")
		list.get(0).name.assertEquals("17.10.16 08:18")
		list.get(1).logFile.name.assertEquals("te-1476732656343.log")
		list.get(1).name.assertEquals("17.10.16 21:30")
	}
	
}