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
package org.testeditor.dsl.common.util

import java.io.File
import org.junit.Test
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.dsl.common.testing.AbstractTest

import static org.mockito.Mockito.*

class MavenExecutorTest extends AbstractTest {

	@InjectMocks
	MavenExecutor mavenExecutor

	@Mock
	OSUtil osUtil

	@Test
	def void testGetExecuteMavenScriptCommandOnWindows() {
		// given
		when(osUtil.isWindows).thenReturn(true)

		// when
		val commandString = mavenExecutor.getExecuteMavenScriptCommand("package", "output=path")

		// then
		assertTrue(commandString.get(0).endsWith("bin\\mvn.bat"))
		assertEquals("package", commandString.get(1))
		assertEquals("-Doutput=path", commandString.get(2))
	}

	@Test
	def void testGetExecuteMavenScriptCommandOnUnix() {
		// given
		when(osUtil.isWindows).thenReturn(false)

		// when
		val commandString = mavenExecutor.getExecuteMavenScriptCommand("package", "output=path")

		// then
		assertTrue(commandString.get(0).endsWith("bin/mvn"))
		assertEquals("package", commandString.get(1))
		assertEquals("-Doutput=path", commandString.get(2))

	}

	@Test
	def void testGetExecuteEmbeddedMavenCommand() {
		// given
		// when
		val commandString = mavenExecutor.getExecuteEmbeddedMavenCommand("package", "path/to/pom", "output=path", true)

		// then
		assertTrue(commandString.get(0).endsWith("bin" + File.separator + "java"))
		assertEquals("-cp", commandString.get(1))
		assertTrue(commandString.contains("output=path"))
		assertTrue(commandString.contains("package"))
		assertTrue(commandString.contains("path/to/pom"))
	}

}
