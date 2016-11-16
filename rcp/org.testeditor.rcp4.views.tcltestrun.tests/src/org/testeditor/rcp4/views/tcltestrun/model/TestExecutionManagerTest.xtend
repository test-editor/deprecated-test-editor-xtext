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
		Files.createFile(new File(dir,"foo.bar").toPath)
		Files.createFile(new File(dir,"te-2016.11.16.-22:24.log").toPath)
		Files.createFile(new File(dir,"te-2016.11.16.-23:24.log").toPath)
		when(logLocationHelper.logLocation).thenReturn(dir)
		
		// when
		val list = testExecutionManager.testExecutionLogs.entries
		
		// then
		list.assertSize(2)
		list.get(0).logFile.name.assertEquals("te-2016.11.16.-23:24.log")
		list.get(0).name.assertEquals("16.11.16 23:24")
		list.get(1).logFile.name.assertEquals("te-2016.11.16.-22:24.log")
		list.get(1).name.assertEquals("16.11.16 22:24")
	}
	
}