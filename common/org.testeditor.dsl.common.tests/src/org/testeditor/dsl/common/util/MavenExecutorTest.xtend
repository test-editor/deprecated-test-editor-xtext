/*******************************************************************************
 * Copyright (c) 2012 - 2018 Signal Iduna Corporation and others.
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

import javax.inject.Inject
import org.junit.Test
import org.testeditor.dsl.common.testing.AbstractTest
import org.testeditor.dsl.common.util.MavenExecutor.MavenVersionValidity

class MavenExecutorTest extends AbstractTest {

	@Inject
	MavenExecutor mavenExecutor

	@Test
	def void testGetScriptCommandOnWindows() {
		// given
		val osIsWindows = true

		// when
		val commandStrings = mavenExecutor.getExecuteMavenScriptCommand("c:\\maven", "test", "output=path", osIsWindows)

		// then
		commandStrings.assertSize(4)
		commandStrings.get(0).assertEquals("c:\\maven\\bin\\mvn.bat")
		commandStrings.get(1).assertEquals("test")
		commandStrings.get(2).assertEquals("-Doutput=path")
		commandStrings.get(3).assertEquals("-V")
	}

	@Test
	def void testGetScriptCommandOnUnix() {
		// given
		val osIsWindows = false

		// when
		val commandStrings = mavenExecutor.getExecuteMavenScriptCommand("/usr/local", "test", "output=path", osIsWindows)

		// then
		commandStrings.assertSize(4)
		commandStrings.get(0).assertEquals("/usr/local/bin/mvn")
		commandStrings.get(1).assertEquals("test")
		commandStrings.get(2).assertEquals("-Doutput=path")
		commandStrings.get(3).assertEquals("-V")
	}

	@Test
	def void testGetScriptCommandWithoutTestParam() {
		// given
		val osIsWindows = false

		// when
		val commandStrings = mavenExecutor.getExecuteMavenScriptCommand("/usr/local", "test", "", osIsWindows)

		// then
		commandStrings.assertSize(3)
		commandStrings.get(0).assertEquals("/usr/local/bin/mvn")
		commandStrings.get(1).assertEquals("test")
		commandStrings.get(2).assertEquals("-V")
	}

	@Test
	def void testGetScriptCommandWithMultipleTargets() {
		// given
		val osIsWindows = false

		// when
		val commandStrings = mavenExecutor.getExecuteMavenScriptCommand("/usr/local", "clean test", null, osIsWindows)

		// then
		commandStrings.assertSize(4)
		commandStrings.get(0).assertEquals("/usr/local/bin/mvn")
		commandStrings.get(1).assertEquals("clean")
		commandStrings.get(2).assertEquals("test")
		commandStrings.get(3).assertEquals("-V")
	}
	
	@Test
	def void testVersionParsing() {
		// given
		val versionString = "ApacheMaven 3.2.5 (and some additional information)"
		
		// when
		val version=mavenExecutor.parseVersionInformation(versionString) 
		
		// then
		version.assertSize(2)
		version.get(0).assertEquals(3)
		version.get(1).assertEquals(2)
	}
	
	@Test
	def void testVersionParsingUnexpectedString() {
		// given
		val versionString = "ApacheMaven 3.x.5 (and some additional information)"
		
		// when
		val version=mavenExecutor.parseVersionInformation(versionString) 
		
		// then
		version.assertNull
	}
	
	@Test
	def void testValidateExpectedVersion() {
		// given
		val version = #[3,2,5]
		
		// when
		val validity=mavenExecutor.validateVersionInformation(version)
		
		// then
		validity.assertEquals(MavenVersionValidity.ok)
	}

	@Test
	def void testValidateUnknownVersion() {
		// given
		val version = null
		
		// when
		val validity=mavenExecutor.validateVersionInformation(version)
		
		// then
		validity.assertEquals(MavenVersionValidity.unknown_version)
	}

	@Test
	def void testValidateVersionToLow() {
		// given
		val version = #[3,1,9]
		
		// when
		val validity=mavenExecutor.validateVersionInformation(version)
		
		// then
		validity.assertEquals(MavenVersionValidity.wrong_version)
	}

}
