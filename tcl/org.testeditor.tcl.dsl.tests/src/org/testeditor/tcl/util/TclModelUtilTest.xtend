package org.testeditor.tcl.util

import java.util.Map
import java.util.Set
import javax.inject.Inject
import org.junit.Before
import org.junit.Test
import org.testeditor.aml.AmlModel
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.Template
import org.testeditor.aml.TemplateVariable
import org.testeditor.aml.Variable
import org.testeditor.aml.dsl.AmlStandaloneSetup
import org.testeditor.aml.dsl.tests.AmlModelGenerator
import org.testeditor.aml.dsl.tests.common.AmlTestModels
import org.testeditor.dsl.common.testing.DummyFixture
import org.testeditor.tcl.AbstractTestStep
import org.testeditor.tcl.MacroCollection
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.dsl.services.TclGrammarAccess
import org.testeditor.tcl.dsl.tests.TclModelGenerator
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest
import org.testeditor.tsl.StepContentValue
import org.testeditor.tsl.StepContentVariable

class TclModelUtilTest extends AbstractParserTest {

	@Inject var TclModelUtil tclModelUtil // class under test
	@Inject extension TclModelGenerator
	@Inject extension AmlModelGenerator
	@Inject var AmlTestModels amlTestModels
	@Inject protected TclGrammarAccess grammarAccess
	@Inject extension ModelUtil modelUtil

	@Before
	def void setup() {		
		(new AmlStandaloneSetup).createInjectorAndDoEMFRegistration
	}

	@Test
	def void testRestoreString() {
		// given
		val testStep = parse('-  <hello>     world "ohoh"   @xyz', grammarAccess.testStepRule, TestStep)
		testStep.contents.get(3).assertInstanceOf(VariableReference)

		// when
		val result = tclModelUtil.restoreString(testStep.contents)

		// then
		result.assertMatches('<hello> world "ohoh" @') // empty variable reference name, since the reference is null
	}

	@Test
	def void restoreStringWithPunctuation() {
		// given
		val questionMark = parse('- Hello World?', grammarAccess.testStepRule, TestStep)
		val questionMarkAndWhitespace = parse('- Hello World  ?', grammarAccess.testStepRule, TestStep)
		val dot = parse('- Hello World  .', grammarAccess.testStepRule, TestStep)
		val dotAndWhitespace = parse('- Hello World.', grammarAccess.testStepRule, TestStep)

		// then
		tclModelUtil.restoreString(questionMark.contents).assertEquals('Hello World?')
		tclModelUtil.restoreString(questionMarkAndWhitespace.contents).assertEquals('Hello World?')
		tclModelUtil.restoreString(dot.contents).assertEquals('Hello World.')
		tclModelUtil.restoreString(dotAndWhitespace.contents).assertEquals('Hello World.')
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
		val someValue = testStep.contents.filter(StepContentVariable).head
		val otherRef = testStep.contents.filter(VariableReference).head
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

	@Test
	def void testCollectDeclaredVariablesTypeMap() {
		// given
		// given
		val amlModel = amlTestModels.dummyComponent(resourceSet)
		amlModel.addToResourceSet
		val tclModel = createComponentTestStepsBasedOnDummy(
			amlModel,
			testStepWithAssignment("newVar", "getValue").withElement("dummyElement"),
			testStepWithAssignment("newMap", "getMap").withElement("dummyElement")
		)
		val testStepContext = tclModel.test.steps.head.contexts.head

		// when		
		val varTypeMap = tclModelUtil.collectDeclaredVariablesTypeMap(testStepContext)
		val varTypeQualifiedName = varTypeMap.get("newVar").qualifiedName
		val mapTypeQualifiedNameWOGenerics = varTypeMap.get("newMap").qualifiedName.replaceFirst("<.*", "")

		// then
		varTypeQualifiedName.assertEquals(String.canonicalName)
		mapTypeQualifiedNameWOGenerics.assertEquals(Map.canonicalName)
		assertSize(varTypeMap.keySet, 2) // only this variable is declared
	}

	@Test
	def void testGetAllTypeUsagesThroughMacroOfEnvVariable() {
		// given
		val envVar = environmentVariables("envVar").head // an environment var
		// given, when, then
		// create calls from a test via a macro to the actual fixture and tests for correct
		// collection of variable types
		testGetAllTypeUsagesOfVariableThroughMacroCalls(envVar)
	}

	@Test
	def void testGetAllTypeUsagesThroughMacroOfTemplateVariable() {
		// given
		// template variables can actually be used only by macros, not within regular test cases
		// ... but since the code is uniform for these cases, any variable reference can do
		// => even references to template variables
		val templateVar = templateVariable("templateVar")

		// given, when, then
		// create calls from a test via a macro to the actual fixture and tests for correct
		// collection of variable types
		testGetAllTypeUsagesOfVariableThroughMacroCalls(templateVar)
	}

	@Test
	def void testGetAllTypeUsagesOfEnvVariable() {
		// given
		val envVar = environmentVariables("envVar").head

		// given, when, then
		// construct a simple test case that calls fixtures with the given var and check whether the types
		// of this variable are determined correctly
		testGetAllTypeUsagesOfVariable(envVar)
	}

	@Test
	def void testGetAllTypeUsagesOfTemplateVariable() {
		// given
		val templateVar = templateVariable("templateVar")

		// given, when, then
		// construct a simple test case that calls fixtures with the given var and check whether the types
		// of this variable are determined correctly
		testGetAllTypeUsagesOfVariable(templateVar)
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
			- Type "something" into <Input>
		''')
		 
		
		// when
		val testStep = tclModel.test.steps.head.contexts.head.steps.filter(TestStep).head
		val result = tclModelUtil.getStepVariableFixtureParameterTypePairs(testStep)

		// then
		result.assertSize(2)
		result.get(0).key.assertInstanceOf(StepContentValue)
		result.get(0).value.qualifiedName.assertEquals("java.lang.String")
		result.get(1).key.assertInstanceOf(StepContentElement)
		result.get(1).value.qualifiedName.assertEquals("java.lang.String")
	}

	/**
	 * create a test that makes a macro call for each step (within one specification step)
	 */
	private def TclModel createMacroTestStepCallsBasedOnDummy(MacroCollection macroCollection,
		AbstractTestStep ... steps) {
		val tclModel = tclModel => [
			test = testCase('Test') => [
				it.steps += specificationStep("spec") => [
					steps.forEach [ step |
						contexts += macroTestStepContext(macroCollection) => [
							it.steps += step
						]
					]
				]
			]
		]
		tclModel.addToResourceSet
		return tclModel
	}

	/** 
	 * create a test that makes performs all steps within one component context (within one specification step)
	 */
	private def TclModel createComponentTestStepsBasedOnDummy(AmlModel amlModel, AbstractTestStep ... steps) {
		val tclModel = tclModel => [
			test = testCase('Test') => [
				it.steps += specificationStep("spec") => [
					contexts += componentTestStepContext(amlModel.components.head) => [
						it.steps.addAll(steps)
					]
				]
			]
		]
		tclModel.addToResourceSet
		return tclModel
	}

	/**
	 * create macros named "spec0..", one for each dummyFixtureMethod that makes a single parameter call.
	 * Each macro will have  template = "step0.." ${param0..} 
	 */
	private def TclModel createMacrosForEachFixtureWithOneParam(AmlModel amlModel, String ... dummyFixtureMethods) {
		val tclModel = tclModel => [
			macroCollection = macroCollection('Test') => [
				dummyFixtureMethods.forEach [ dummyFixtureMethod, index |
					macros += macro("spec") => [
						template = template("step" + index).withParameter("param" + index)
						val templateVar = template.contents.last as TemplateVariable
						contexts += componentTestStepContext(amlModel.components.head) => [
							it.steps += testStep(dummyFixtureMethod).withReferenceToVariable(templateVar)
						]
					]
				]
			]
		]
		tclModel.addToResourceSet
		return tclModel
	}

	/**
	 * create macros named "spec0..", one for each macro passed that makes a single parameter call.
	 * Each macro will have  template = "stepNext0.." ${paramNext0..} 
	 */
	private def TclModel createMacrosForEachMacroWithOneParameter(MacroCollection macroCollection,
		String ... dummyFixtureMethods) {
		val tclModel = tclModel => [
			it.macroCollection = macroCollection('TestNext') => [
				dummyFixtureMethods.forEach [ dummyFixtureMethod, index |
					macros += macro("MacroNext" + index) => [
						template = template("stepNext" + index).withParameter("paramNext" + index)
						val templateVar = template.contents.last as TemplateVariable
						contexts += macroTestStepContext(macroCollection) => [
							it.steps += testStep("step" + index).withReferenceToVariable(templateVar)
						]
					]
				]
			]
		]
		tclModel.addToResourceSet
		return tclModel
	}

	/**
	 * create macros for DummyFixture.start and DummyFixture.wait, then generate a test case 
	 * that calls these macros (within one specification step).
	 * Getting the type for these parameters needs to resolve parameters to the macros
	 * and usage of those parameters within the macro in order to retrieve the right type
	 */
	private def void testGetAllTypeUsagesOfVariableThroughMacroCalls(Variable variable) {
		// given
		val amlModel = amlTestModels.dummyComponent(resourceSet)
		amlModel.addToResourceSet
		val macroModel = createMacrosForEachFixtureWithOneParam(amlModel, "start", "wait")
		val macroModelToCallMacros = createMacrosForEachMacroWithOneParameter(macroModel.macroCollection, "step0",
			"step1")
		val tclModel = createMacroTestStepCallsBasedOnDummy(
			macroModelToCallMacros.macroCollection,
			testStep("stepNext0").withReferenceToVariable(variable),
			testStep("stepNext1").withReferenceToVariable(variable)
		)
		val testStepContext1 = tclModel.test.steps.head.contexts.head
		val testStepContext2 = tclModel.test.steps.head.contexts.last

		// when
		val result1 = tclModelUtil.getAllTypeUsagesOfVariable(testStepContext1, variable.name)
		val result2 = tclModelUtil.getAllTypeUsagesOfVariable(testStepContext2, variable.name)

		// then
		val qualifiedNames1 = result1.map[qualifiedName].toSet
		val qualifiedNames2 = result2.map[qualifiedName].toSet
		qualifiedNames1.assertEquals(#{String.canonicalName})
		qualifiedNames2.assertEquals(#{long.canonicalName})
	}

	/**
	 * test whether the types of the given variable are determined correctly if they are used
	 * in direct calls to fixtures (no macros) 
	 */
	private def void testGetAllTypeUsagesOfVariable(Variable variable) {
		// given
		val amlModel = amlTestModels.dummyComponent(resourceSet)
		amlModel.addToResourceSet
		val tclModel = createComponentTestStepsBasedOnDummy(
			amlModel,
			testStep("start").withReferenceToVariable(variable),
			testStep("wait").withReferenceToVariable(variable)
		)
		val testStepContext = tclModel.test.steps.head.contexts.head

		// when
		val result = tclModelUtil.getAllTypeUsagesOfVariable(testStepContext, variable.name)

		// then
		val qualifiedNames = result.map[qualifiedName].toSet
		qualifiedNames.assertEquals(#{long.canonicalName, String.canonicalName})
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
