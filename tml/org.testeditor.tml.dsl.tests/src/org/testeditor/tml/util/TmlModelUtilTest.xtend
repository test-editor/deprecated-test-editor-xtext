package org.testeditor.tml.util

import javax.inject.Inject
import org.junit.Test
import org.testeditor.aml.Template
import org.testeditor.tml.MacroTestStepContext
import org.testeditor.tml.StepContentVariableReference
import org.testeditor.tml.TestStep
import org.testeditor.tml.dsl.tests.parser.AbstractParserTest
import org.testeditor.tsl.StepContentVariable

class TmlModelUtilTest extends AbstractParserTest {

	@Inject var TmlModelUtil tmlModelUtil // class under test

	@Test
	def void testRestoreString() {
		// given
		val testStep = parse('-  <hello>     world "ohoh"   @xyz', grammarAccess.testStepRule, TestStep)
		testStep.contents.get(3).assertInstanceOf(StepContentVariableReference)
		testStep.contents.join(" ")[value].assertMatches("hello world ohoh .+")

		// when
		val result = tmlModelUtil.restoreString(testStep.contents)

		// then
		result.assertMatches('<hello> world "ohoh" @')
	}

	@Test
	def void testFindMacroDefinition() {
		// given
		val macroCollection = parse( '''
			package com.example
			
			# MyMacroCollection
			
			## MacroStartWith
			template = "start with" ${startparam}
			Component: MyComponent
			- put @startparam into <other>
			
			## MacroUseWith
			template = "use macro with" ${useparam}
			Macro: MyMacroCollection
			- start with @useparam						
		''')
		val macroCalled = macroCollection.macros.head
		val macroCall = macroCollection.macros.last
		val macroTestStepContext = macroCall.contexts.head as MacroTestStepContext

		// when
		val macro = tmlModelUtil.findMacroDefinition(macroTestStepContext)

		// then
		macro.assertSame(macroCalled)
	}

	@Test
	def void testNormalizeTemplate() {
		// given
		val template = parse('''
			"start with" ${somevar} "and more" ${othervar}
		''', grammarAccess.templateRule, Template)

		// when
		val normalizedTemplate = tmlModelUtil.normalize(template)

		// then
		normalizedTemplate.assertEquals('start with "" and more ""')
	}

	@Test
	def void testNormalizeTestStep() {
		// given
		val testStep = parse('''
			- start with "some" and more @other
		''', grammarAccess.testStepRule, TestStep)

		// when
		val normalizedTestStep = tmlModelUtil.normalize(testStep)

		// then
		normalizedTestStep.assertEquals('start with "" and more ""')
	}

	@Test
	def void testVariableToValueMapping() {
		// given
		val testStep = parse('''
			- start with "some" and more @other
		''', grammarAccess.testStepRule, TestStep)

		val template = parse('''
			"start with" ${somevar} "and more" ${othervar}
		''', grammarAccess.templateRule, Template)
		val somevar = template.contents.get(1)
		val othervar = template.contents.get(3)

		// when
		val varValueMap = tmlModelUtil.getVariableToValueMapping(testStep, template)

		// then
		varValueMap.keySet.assertSize(2)
		varValueMap.get(somevar).assertInstanceOf(StepContentVariable).value.assertEquals("some")
		varValueMap.get(othervar).assertInstanceOf(StepContentVariableReference)
	}

}
