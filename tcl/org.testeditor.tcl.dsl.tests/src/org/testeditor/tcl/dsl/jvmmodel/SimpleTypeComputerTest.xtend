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
import org.testeditor.dsl.common.testing.DummyEnum
import org.testeditor.dsl.common.testing.DummyFixture
import org.testeditor.fixture.core.MaskingString
import org.testeditor.tcl.Macro
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest
import org.testeditor.tsl.StepContentValue

import static java.util.Optional.*

class SimpleTypeComputerTest extends AbstractParserTest {

	@Inject SimpleTypeComputer typeComputer // class under test

	AmlModel aml

	@Before
	def void setup() {
		aml = parseAml(DummyFixture.amlModel)
	}

	@Test
	def void interactionWithNoParameter() {
		// given
		val interaction = aml.interactionTypes.findFirst[name == "stop"].assertNotNull

		// when
		val variablesWithTypes = typeComputer.getVariablesWithTypes(interaction)

		// then
		variablesWithTypes.entrySet.assertEmpty
	}

	@Test
	def void interactionWithPrimitiveTypeParameter() {
		// given
		val interaction = aml.interactionTypes.findFirst[name == "wait"].assertNotNull

		// when
		val variablesWithTypes = typeComputer.getVariablesWithTypes(interaction)

		// then
		variablesWithTypes.entrySet.assertSingleElement => [
			key.name.assertEquals("seconds")
			value.get.qualifiedName.assertEquals('long')
		]
	}

	@Test
	def void macroWithNoParameter() {
		// given
		val macro = parseMacro('''
			template = "Do nothing"
		''')

		// when
		val variablesWithTypes = typeComputer.getVariablesWithTypes(macro)

		// then
		variablesWithTypes.entrySet.assertEmpty
	}

	@Test
	def void macroWithNoParameterButInternalVariableUsage() {
		// given
		val macro = parseMacro('''
			template = "Read and write"
			Component: GreetingApplication
			- value = Read value from <bar>
			- Set value of <Input> to @value
		''')

		// when
		val variablesWithTypes = typeComputer.getVariablesWithTypes(macro)

		// then
		variablesWithTypes.entrySet.assertEmpty
	}

	@Test
	def void macroWithUnusedParameter() {
		// given
		val macro = parseMacro('''
			template = "Do nothing with" ${unused}
		''')

		// when
		val variablesWithTypes = typeComputer.getVariablesWithTypes(macro)

		// then
		variablesWithTypes.entrySet.assertSingleElement => [
			key.name.assertEquals("unused")
			value.assertEquals(empty)
		]
	}

	@Test
	def void macroWithLongParameter() {
		// given
		val macro = parseMacro('''
			template = "Wait for" ${x} "seconds"
			Component: GreetingApplication
			- Wait for @x seconds
		''')

		// when
		val variablesWithTypes = typeComputer.getVariablesWithTypes(macro)

		// then
		variablesWithTypes.entrySet.assertSingleElement => [
			key.name.assertEquals("x")
			value.get.qualifiedName.assertEquals('long')
		]
	}

	@Test
	def void macroWithConfidentialParameter() {
		// given
		val macro = parseMacro('''
			template = "Process some confidential information" ${x} 
			Component: GreetingApplication
			- Type confidential @x into <Input>
		''')

		// when
		val variablesWithTypes = typeComputer.getVariablesWithTypes(macro)

		// then
		variablesWithTypes.entrySet.assertSingleElement => [
			key.name.assertEquals("x")
			value.get.qualifiedName.assertEquals(MaskingString.name)
		]
	}

	@Test
	def void macroWithUnusedStringAndLongParameter() {
		// given
		val macro = parseMacro('''
			template = "Do nothing with" ${unused} "write" ${value} "and wait for" ${seconds} "seconds"
			Component: GreetingApplication
			- Set value of <Input> to @value
			- Wait for @seconds seconds
		''')

		// when
		val variablesWithTypes = typeComputer.getVariablesWithTypes(macro)

		// then
		variablesWithTypes.entrySet.assertSize(3) => [
			get(0) => [
				key.name.assertEquals('unused')
				value.assertEquals(empty)
			]
			get(1) => [
				key.name.assertEquals('value')
				value.get.qualifiedName.assertEquals(String.name)
			]
			get(2) => [
				key.name.assertEquals('seconds')
				value.get.qualifiedName.assertEquals('long')
			]
		]
	}

	@Test
	def void macroWithNestedMacroParameter() {
		// given
		val macro = parseMacro('''
			template = "Sleep nested for" ${x} "seconds"
			Macro: MyMacroCollection
			- Sleep for @x seconds
			
			## SleepMacro
			template = "Sleep for" ${seconds} "seconds"
			Component: GreetingApplication
			- Wait for @seconds seconds
		''')

		// when
		val variablesWithTypes = typeComputer.getVariablesWithTypes(macro)

		// then
		variablesWithTypes.entrySet.assertSingleElement => [
			key.name.assertEquals("x")
			value.get.qualifiedName.assertEquals('long')
		]
	}

	@Test
	def void macroWithUnresolvedNestedInteraction() {
		// given
		val macro = parseMacro('''
			template = "Do something with" ${x}
			Component: GreetingApplication
			- Unresolved interaction using @x
		''')

		// when
		val variablesWithTypes = typeComputer.getVariablesWithTypes(macro)

		// then
		variablesWithTypes.entrySet.assertSingleElement => [
			key.name.assertEquals("x")
			value.assertEquals(empty)
		]
	}

	@Test
	def void macroWithUnresolvedNestedMacro() {
		// given
		val macro = parseMacro('''
			template = "Do something with" ${x}
			Macro: MyMacroCollection
			- Unresolved macro using @x
		''')

		// when
		val variablesWithTypes = typeComputer.getVariablesWithTypes(macro)

		// then
		variablesWithTypes.entrySet.assertSingleElement => [
			key.name.assertEquals("x")
			value.assertEquals(empty)
		]
	}

	@Test
	def void interactionWithLocatorStrategyInNonLastPosition() {
		// given (interaction with three parameters: element, locatorStrategy, value
		val interaction = aml.interactionTypes.findFirst[name == "typeInto"].assertNotNull

		// when (called with two parameters, since template defines two, and locatorStrategy is automatically added)
		val variablesWithTypes = typeComputer.getVariablesWithTypes(interaction)

		// then (make sure that the two parameters are returned with their respective type, skipping locatorStrategy)
		variablesWithTypes.keySet => [
			assertSize(2)
			findFirst[name == "element"].assertNotNull => [
				variablesWithTypes.get(it).get.type.qualifiedName.assertEquals("java.lang.String")
			]
			findFirst[name == "value"].assertNotNull => [
				variablesWithTypes.get(it).get.type.qualifiedName.assertEquals("java.lang.String")
			]
		]
	}

	@Test
	def void testJavaEnumFixtureParameter() {
		// given
		parseAml(DummyFixture.amlModel)
		
		val tclModel = parseTcl('''
			package com.example
			
			# MyTest
			
			* some fixture usage
			Component: GreetingApplication
			- Set enum of <Input> to "enum_a"
		''')

		// when
		val testStep = tclModel.test.steps.head.contexts.head.steps.filter(TestStep).last
		val stepContentTypePairs = typeComputer.getStepVariableFixtureParameterTypePairs(testStep)

		// then
		stepContentTypePairs.assertSize(2) => [
			get(0) => [
				key.assertInstanceOf(StepContentElement)
				value.get.qualifiedName.assertEquals(String.name)
			]
			get(1) => [
				key.assertInstanceOf(StepContentValue)
				value.get.qualifiedName.assertEquals(DummyEnum.name)
			]
		]
	}

	@Test
	def void testStepVariableFixtureParameterTypePairs() {
		// given
		parseAml(DummyFixture.amlModel)

		val tclModel = parseTcl('''
			package com.example
			
			# MyTest
			
			* some fixture usage
			Component: GreetingApplication
			- TypeLong "42" into <Input>  // maps to: DummyFixture.typeInto(String element, Enum locatorStrategy, long value)
		''')
		 
		
		// when
		val testStep = tclModel.test.steps.head.contexts.head.steps.filter(TestStep).last
		val stepContentTypePairs = typeComputer.getStepVariableFixtureParameterTypePairs(testStep)

		// then
		stepContentTypePairs.assertSize(2)
		stepContentTypePairs.get(0).key.assertInstanceOf(StepContentValue)
		stepContentTypePairs.get(0).value.get.qualifiedName.assertEquals(long.name)
		stepContentTypePairs.get(1).key.assertInstanceOf(StepContentElement)
		stepContentTypePairs.get(1).value.get.qualifiedName.assertEquals(String.name)
	}

	private def Macro parseMacro(String macro) {
		val tcl = '''
			package com.example
			
			# MyMacroCollection
			
			## TheMacro
			«macro»
		'''
		val tclModel = tcl.parseTcl('MyMacroCollection.tml')
		tclModel.assertNoErrors
		return tclModel.macroCollection.macros.findFirst[name == "TheMacro"]
	}

}
