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

import javax.inject.Inject
import org.junit.Before
import org.junit.Test
import org.testeditor.aml.AmlModel
import org.testeditor.dsl.common.testing.DummyFixture
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest

class VariableCollectorTest extends AbstractParserTest {
	
	@Inject VariableCollector variableCollector // class under test

	AmlModel aml

	@Before
	def void setup() {
		aml = parseAml(DummyFixture.amlModel)
	}

	@Test
	def void testCollectDeclaredVariablesTypeMap() {
		// given
		val tcl = '''
			package com.example
			
			# MyTest
			
			* do some
				Component: GreetingApplication
				- boolVar = Read long from <bar>
		'''
		val tclModel = tcl.parseTcl('MyTest.tcl')
		tclModel.assertNoErrors
		
		val declaredVariables = variableCollector.collectDeclaredVariablesTypeMap(tclModel.test.steps.head.contexts.head)

		declaredVariables.keySet.assertSize(1)
		declaredVariables.get("boolVar").qualifiedName.assertEquals(long.name)
	}

	
}