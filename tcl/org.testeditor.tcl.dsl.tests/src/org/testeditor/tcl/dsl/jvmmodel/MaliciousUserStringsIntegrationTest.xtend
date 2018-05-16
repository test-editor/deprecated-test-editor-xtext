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
package org.testeditor.tcl.dsl.jvmmodel

import org.junit.Before
import org.junit.Test
import org.testeditor.dsl.common.testing.DummyFixture

class MaliciousUserStringsIntegrationTest extends AbstractTclGeneratorIntegrationTest {

	@Before
	def void parseAmlModel() {
		parseAml(DummyFixture.amlModel)
	}

	@Test
	def void escapesUserStringInReporting() {
		// given
		val tcl = '''
			* Some test step ";System.exit(0);//"
		'''

		// when
		val generatedCode = tcl.parseAndGenerate

		// then
		generatedCode.assertContains('''reporter.enter(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "Some test step \";System.exit(0);//\"", IDvar0, TestRunReporter.Status.STARTED, variables());''')
	}

	@Test
	def void escapesUserStringInAssertion() {
		// given
		val tcl = '''
			* My test
			
				Mask: GreetingApplication
				- x = Read long from <bar>
				- assert x > '0"));\nSystem.exit(0);//'
		'''

		// when
		val generatedCode = tcl.parseAndGenerate

		// then
		generatedCode.assertContains('''
			assertTrue("SimpleTest.tcl:9: x > '0\"));\\nSystem.exit(0);//'", x > Long.parseLong("0\"));\nSystem.exit(0);//"));
		''')
	}

	@Test
	def void escapesUserStringInJsonParsing() {
		// given
		val tcl = '''
			* My test
			
				Mask: GreetingApplication
				- json = Read jsonObject from <bar>
				- json.y = {
					"key": 'value"));System.exit(0);'
				}
		'''

		// when
		val generatedCode = tcl.parseAndGenerate

		// then
		generatedCode.assertContains('''
			json.getAsJsonObject().add("y", new com.google.gson.JsonParser().parse("{\n\t\t\"key\":'value\"));System.exit(0);'\n\t}"));
		''')
	}

	@Test
	def void escapesUserStringInJsonAssignment() {
		// given
		val tcl = '''
			* My test
			
				Mask: GreetingApplication
				- json = Read jsonObject from <bar>
				- json.'y", null);\nSystem.exit(0);//' = {}
		'''

		// when
		val generatedCode = tcl.parseAndGenerate

		// then
		generatedCode.assertContains('''
			json.getAsJsonObject().add("y\", null);\nSystem.exit(0);//", new com.google.gson.JsonParser().parse("{}"));
		''')
	}

	@Test
	def void escapesUserStringInJsonRetrieval() {
		// given
		val tcl = '''
			* Test assertions in the famous greeting application
				Mask: GreetingApplication
				- json = Read jsonObject from <bar>
				- assert json.'key"));\nSystem.exit(0);//'
		'''

		// when
		val generatedCode = tcl.parseAndGenerate

		// then
		generatedCode.assertContains('''
			assertNotNull("SimpleTest.tcl:8: json.'key\"));\\nSystem.exit(0);//'", json.getAsJsonObject().get("key\"));\nSystem.exit(0);//"));
		''')
	}
	
	@Test
	def void escapesUserStringInLocator() {
		// given
		val aml = '''
			package com.example
			
			component MyApplication is Application {
				element evil is Label {
					locator = 'locator");\nSystem.exit(0);//'
				}
			}
		'''
		val tcl = '''
			* Example
				Mask: MyApplication
				- Read value from <evil>
		'''

		// when
		aml.parseAml
		val generatedCode = tcl.parseAndGenerate

		// then
		generatedCode.assertContains('''
			dummyFixture.getValue("locator\");\nSystem.exit(0);//");
		''')
	}

}
