package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.junit.Before
import org.junit.Test
import org.testeditor.aml.AmlModel
import org.testeditor.dsl.common.testing.DummyFixture
import org.testeditor.tcl.Macro
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest

import static java.util.Optional.*

class SimpleTypeComputerTest extends AbstractParserTest {

	@Inject SimpleTypeComputer typeComputer

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
		
		val declaredVariables = typeComputer.collectDeclaredVariablesTypeMap(tclModel.test.steps.head.contexts.head)

		declaredVariables.keySet.assertSize(1)
		declaredVariables.get("boolVar").qualifiedName.assertEquals(long.name)
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
