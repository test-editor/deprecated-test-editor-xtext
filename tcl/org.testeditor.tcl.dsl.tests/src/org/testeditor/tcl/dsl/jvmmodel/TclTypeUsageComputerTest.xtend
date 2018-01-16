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

import com.google.gson.JsonObject
import javax.inject.Inject
import org.eclipse.xtext.EcoreUtil2
import org.junit.Test
import org.testeditor.aml.AmlModel
import org.testeditor.aml.TemplateVariable
import org.testeditor.aml.Variable
import org.testeditor.aml.dsl.tests.AmlModelGenerator
import org.testeditor.aml.dsl.tests.common.AmlTestModels
import org.testeditor.tcl.AbstractTestStep
import org.testeditor.tcl.MacroCollection
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.dsl.services.TclGrammarAccess
import org.testeditor.tcl.dsl.tests.TclModelGenerator
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest

class TclTypeUsageComputerTest extends AbstractParserTest {

	@Inject TclTypeUsageComputer typeUsageComputer // class under test
	@Inject extension TclModelGenerator
	@Inject extension AmlModelGenerator
	@Inject AmlTestModels amlTestModels
	@Inject protected TclGrammarAccess grammarAccess
	
	@Test
	def void testGetTypeUsagesForAssignments() {
		// given
		val tclModel = tclModel => [
			it.test = testCase("myTest") => [
				it.steps += specificationStep("my", "test") => [
					contexts += componentTestStepContext(null) => [
						val assignment = testStepWithAssignment("jsonVar", "Read", "jsonObject", "from").
							withElement("bar")
						steps += assignment
						steps += assignmentThroughPath(assignment.variable, "key", "nextKey") => [
							expression = jsonObjectWithStringKeyValue("key", "value")
						]
					]
				]
			]
		]
		val context = EcoreUtil2.getAllContentsOfType(tclModel, TestStepContext).head
		val expectedTypes = #[JsonObject.name]

		// when
		val usage = typeUsageComputer.getAllPossibleTypeUsagesOfVariable(context, "jsonVar")

		// then
		usage.assertSize(expectedTypes.size)
		usage.forEach [
			present.assertTrue
			val typeName = get.qualifiedName.replaceAll("<.*", "") // make sure to remove type parameter
			expectedTypes.contains(typeName).
				assertTrue('''Type «typeName» not found in expected list «expectedTypes»''')
		]
	}

	@Test
	def void testGetAllTypeUsagesThroughMacroOfEnvVariable() {
		// given
		val envVar = environmentVariablesPublic("envVar").head // an environment var
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
		val envVar = environmentVariablesPublic("envVar").head

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
		val result1 = typeUsageComputer.getAllPossibleTypeUsagesOfVariable(testStepContext1, variable.name)
		val result2 = typeUsageComputer.getAllPossibleTypeUsagesOfVariable(testStepContext2, variable.name)

		// then
		val qualifiedNames1 = result1.map[get.qualifiedName].toSet
		val qualifiedNames2 = result2.map[get.qualifiedName].toSet
		qualifiedNames1.assertEquals(#{String.canonicalName})
		qualifiedNames2.assertEquals(#{long.canonicalName})
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
		val result = typeUsageComputer.getAllPossibleTypeUsagesOfVariable(testStepContext, variable.name)

		// then
		val qualifiedNames = result.map[get.qualifiedName].toSet
		qualifiedNames.assertEquals(#{long.canonicalName, String.canonicalName})
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

}
