package org.testeditor.tcl.util

import java.util.Map
import java.util.Set
import com.google.inject.Inject
import org.junit.Before
import org.junit.Test
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.Template
import org.testeditor.aml.dsl.AmlStandaloneSetup
import org.testeditor.aml.dsl.tests.AmlModelGenerator
import org.testeditor.tcl.AbstractTestStep
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.dsl.services.TclGrammarAccess
import org.testeditor.tcl.dsl.tests.TclModelGenerator
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest
import org.testeditor.tsl.StepContentVariable

class TclModelUtilTest extends AbstractParserTest {

	@Inject var TclModelUtil tclModelUtil // class under test
	
	@Inject extension TclModelGenerator
	@Inject extension AmlModelGenerator

	@Inject ModelUtil modelUtil	
	@Inject TclGrammarAccess grammarAccess

	@Before
	def void setup() {		
		(new AmlStandaloneSetup).createInjectorAndDoEMFRegistration
	}

	@Test
	def void testRestoreString() {
		// given
		val testStep = parse('-  some <hello>     world "ohoh"   @xyz', grammarAccess.testStepRule, TestStep)
		testStep.fixtureReference.contents.get(4).assertInstanceOf(VariableReference)

		// when
		val result = tclModelUtil.restoreString(testStep.fixtureReference.contents)

		// then
		result.assertMatches('some <hello> world "ohoh" @') // empty variable reference name, since the reference is null
	}

	@Test
	def void restoreStringWithPunctuation() {
		// given
		val questionMark = parse('- Hello World?', grammarAccess.testStepRule, TestStep)
		val questionMarkAndWhitespace = parse('- Hello World  ?', grammarAccess.testStepRule, TestStep)
		val dot = parse('- Hello World  .', grammarAccess.testStepRule, TestStep)
		val dotAndWhitespace = parse('- Hello World.', grammarAccess.testStepRule, TestStep)

		// when, then
		tclModelUtil.restoreString(questionMark.fixtureReference.contents).assertEquals('Hello World?')
		tclModelUtil.restoreString(questionMarkAndWhitespace.fixtureReference.contents).assertEquals('Hello World?')
		tclModelUtil.restoreString(dot.fixtureReference.contents).assertEquals('Hello World.')
		tclModelUtil.restoreString(dotAndWhitespace.fixtureReference.contents).assertEquals('Hello World.')
	}

	@Test
	def void testFindMacroDefinition() {
		// given
		val tmlModel = parseTcl( '''
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
		val macroCalled = tmlModel.macroCollection.macros.head
		val macroCall = tmlModel.macroCollection.macros.last
		val macroTestStepContext = macroCall.contexts.head as MacroTestStepContext

		// when
		val macro = tclModelUtil.findMacroDefinition(macroTestStepContext.steps.filter(TestStep).head, macroTestStepContext)

		// then
		macro.assertSame(macroCalled)
	}

	@Test
	def void testNormalizeTemplate() {
		// given
		val template = parse('''
			"start with" ${somevar} "and more" ${othervar} "?"
		''', grammarAccess.templateRule, Template)

		// when
		val normalizedTemplate = modelUtil.normalize(template)

		// then
		normalizedTemplate.assertEquals('start with "" and more ""?')
	}

	@Test
	def void testNormalizeTestStep() {
		// given
		val testStep = parse('''
			- start with "some" and more @other ?
		''', grammarAccess.testStepRule, TestStep)

		// when
		val normalizedTestStep = tclModelUtil.normalize(testStep)

		// then
		normalizedTestStep.assertEquals('start with "" and more ""?')
	}

	@Test
	def void testStepContentToTemplateVariablesMapping() {
		// given
		val testStep = parse('''
			- start with "some" and more @other
		''', grammarAccess.testStepRule, TestStep)
		val template = parse('''
			"start with" ${somevar} "and more" ${othervar}
		''', grammarAccess.templateRule, Template)
		val someValue = testStep.fixtureReference.contents.filter(StepContentVariable).head
		val otherRef = testStep.fixtureReference.contents.filter(VariableReference).head
		val somevar = template.contents.get(1)
		val othervar = template.contents.get(3)

		// when
		val map = tclModelUtil.getStepContentToTemplateVariablesMapping(testStep, template)

		// then
		map.entrySet.assertSize(2)
		map.get(someValue).assertSame(somevar)
		map.get(otherRef).assertSame(othervar)
	}

	@Test
	def void testMakesUseOfVariablesViaReference_TemplateVariableInStep() {
		// given
		val variable = templateVariable("var") // a template var
		val step = testStep("some").withReferenceToVariable(variable) // a test step using that var
		// when, then
		// create some complete model (test case, ....) around the test step and check that it is actually using this var!
		testMakesUseOfVariablesViaReference_VariableInStep("var", step)
	}

	@Test
	def void testMakesUseOfVariablesViaReference_TemplateVariableInAssertion() {
		// given
		val variable = templateVariable("var") // a template var
		val step = assertionTestStep => [
			// an assertion using this var
			assertExpression = compareMatching(variableReference => [it.variable = variable], "compared-with")
		]

		// when, then
		// create some complete model (test case, ....) around the assertion step and check that it is actually using this var!
		testMakesUseOfVariablesViaReference_VariableInStep("var", step)
	}

	@Test
	def void testMakesUseOfVariablesViaReference_EnvironmentVariableInStep() {
		// given
		val variable = environmentVariables("var").head // an environment var
		val step = testStep("some").withReferenceToVariable(variable) // a test step using that var
		// when, then
		// create some complete model (test case, ....) around the test step and check that it is actually using this var!
		testMakesUseOfVariablesViaReference_VariableInStep("var", step)
	}

	@Test
	def void testMakesUseOfVariablesViaReference_EnvironmentVariableInAssertion() {
		// given
		val variable = environmentVariables("var").head // an environment var
		val step = assertionTestStep => [
			// an assertion using this var
			assertExpression = compareMatching(variableReference => [it.variable = variable], "compared-with")
		]

		// when, then
		// create some complete model (test case, ....) around the assertion step and check that it is actually using this var!
		testMakesUseOfVariablesViaReference_VariableInStep("var", step)
	}

	/**
	 * test that the step passed is identified correctly for using the variable.
	 * test also that this step is not identified to be using variables that differ from the one passed. 
	 */
	private def void testMakesUseOfVariablesViaReference_VariableInStep(String variableName, AbstractTestStep step) {
		val context = componentTestStepContext(null) => [
			steps += step
		]

		val nonMatchingVariableName1 = variableName + "A"
		val nonMatchingVariableName2 = "B" + variableName
		val expectations = #{
			#{variableName} -> true,
			#{nonMatchingVariableName1, variableName, nonMatchingVariableName2} -> true,
			#{} -> false,
			#{nonMatchingVariableName1, nonMatchingVariableName2} -> false
		}

		// when
		val results = newHashMap
		expectations.forEach[key, value|results.put(key, tclModelUtil.makesUseOfVariablesViaReference(context, key))]

		// then
		results.assertEquals(expectations)
	}

	private def <K, V> void assertEquals(Map<K, V> actualMap, Map<K, V> expectedMap) {
		assertEquals(actualMap.keySet, expectedMap.keySet, "key set differs")
		actualMap.forEach [key, value|
			assertEquals(expectedMap.get(key),
				value, '''value of key='«key»' differ (expected='«expectedMap.get(key)»' != actual='«actualMap.get(key)»').''')
		]
	}

	private def <V> void assertEquals(Set<V> actualSet, Set<V> expectedSet, String message) {
		assertEquals(actualSet.size, expectedSet.size, 'set size differs: ' + message)
		actualSet.forEach [
			assertTrue(expectedSet.contains(it), '''expected set does not contain value='«it»': ''' + message)
		]
	}

}
