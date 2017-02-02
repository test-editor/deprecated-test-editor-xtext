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
import org.testeditor.dsl.common.testing.AbstractTest

class MavenExecutorTest extends AbstractTest {

	@InjectMocks
	MavenExecutor mavenExecutor

	@Test
	def void testGetExecuteMavenScriptCommandOnWindows() {
		// given
		val osIsWindows = true

		// when
		val commandString = mavenExecutor.getExecuteMavenScriptCommand("package", "output=path", osIsWindows)

		// then
		commandString.get(0).endsWith("bin\\mvn.bat").assertTrue
		commandString.get(1).assertEquals("package")
		commandString.get(2).assertEquals("-Doutput=path")
	}

	@Test
	def void testGetExecuteMavenScriptCommandOnUnix() {
		// given
		val osIsWindows = false

		// when
		val commandString = mavenExecutor.getExecuteMavenScriptCommand("package", "output=path", osIsWindows)

		// then
		commandString.get(0).endsWith("bin/mvn").assertTrue
		commandString.get(1).assertEquals("package")
		commandString.get(2).assertEquals("-Doutput=path")

	}


}
