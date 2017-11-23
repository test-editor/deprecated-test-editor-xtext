/*******************************************************************************
 * Copyright (c) 2012 - 2017 Signal Iduna Corporation and others.
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

import com.google.gson.JsonObject
import javax.inject.Inject
import org.eclipse.xtext.EcoreUtil2
import org.junit.Before
import org.junit.Test
import org.testeditor.dsl.common.testing.DummyFixture
import org.testeditor.fixture.core.MaskingString
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest

class VariableCollectorTest extends AbstractParserTest {

	@Inject VariableCollector variableCollector // class under test

	@Before
	def void setup() {
		parseAml(DummyFixture.amlModel)
	}

	@Test
	def void testCollectDeclaredVariablesTypeMap() {
		// given
		val tcl = '''
			package com.example
			
			# MyTest
			
			* do some
				Component: GreetingApplication
				- longVar = Read long from <bar>
				- boolVar = Read bool from <bar>
				- jsonVar = Read jsonObject from <bar>
				- Is <bar> visible?                 // no assignment test step
				- Read value from <bar>             // no assignment of value
				- stringVar = Read value from <bar>
				- confidentialVar = Read confidential information from <bar>
		'''
		val tclModel = tcl.parseTcl('MyTest.tcl')
		tclModel.assertNoErrors

		// when
		val context = EcoreUtil2.getAllContentsOfType(tclModel, TestStepContext).head
		val declaredVariables = variableCollector.collectDeclaredVariablesTypeMap(context)

		// then
		declaredVariables.keySet.assertSize(5)
		declaredVariables.get("longVar").qualifiedName.assertEquals(long.name)
		declaredVariables.get("boolVar").qualifiedName.assertEquals(boolean.name)
		declaredVariables.get("jsonVar").qualifiedName.assertEquals(JsonObject.name)
		declaredVariables.get("stringVar").qualifiedName.assertEquals(String.name)
		declaredVariables.get("confidentialVar").qualifiedName.assertEquals(MaskingString.name)
	}

}
