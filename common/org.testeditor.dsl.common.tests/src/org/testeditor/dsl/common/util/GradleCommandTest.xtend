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

import org.junit.Test
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.dsl.common.testing.AbstractTest

import static org.mockito.Mockito.*

class GradleCommandTest extends AbstractTest {

	@InjectMocks
	GradleCommand gradleCommand;

	@Mock
	OSUtil osUtil;

	@Test
	def void testGetWindowsCommand() {
		// given
		when(osUtil.isWindows).thenReturn(true)

		// when
		val commandString = gradleCommand.getCommandString("package")

		// then
		assertEquals("gradlew.bat", commandString.get(0))
		assertEquals("package", commandString.get(1))
	}

	@Test
	def void testGetUnixCommand() {
		// given
		when(osUtil.isWindows).thenReturn(false)

		// when
		val commandString = gradleCommand.getCommandString("clean", "package")

		// then
		assertEquals("gradlew", commandString.get(0))
		assertEquals("clean", commandString.get(1))
		assertEquals("package", commandString.get(2))
	}

}
