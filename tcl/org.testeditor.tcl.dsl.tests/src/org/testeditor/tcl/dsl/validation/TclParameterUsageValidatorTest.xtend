/*******************************************************************************
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
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
package org.testeditor.tcl.dsl.validation

import javax.inject.Inject
import org.junit.Test
import org.testeditor.aml.dsl.tests.AmlModelGenerator
import org.testeditor.tcl.Macro
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.tests.TclModelGenerator

import static org.testeditor.tcl.TclPackage.Literals.*

class TclParameterUsageValidatorTest extends AbstractUnmockedTclValidatorTest {
	
	@Inject extension AmlModelGenerator
	@Inject extension TclModelGenerator

	@Test
	def void testDirectCallVariableTypeChecks() {
		val macroModel = tclModel => [
			macroCollection = macroCollection("MacroCollection") => [
				// macro calls (directly) the aml interaction "start" (which expects the parameter to be of type String)
				macros += macro("MyCallMacro") => [
					template = template("mycall").withParameter("appname")
					contexts += componentTestStepContext(dummyComponent) => [
						steps += testStep("start").withReferenceToTemplateVariable("appname")
					]
				]
				// macro calls (directly) the aml interaction "wait" (which expects the parameter to be of type long)
				macros += macro("OtherCallMacro") => [
					template = template("othercall").withParameter("secs")
					contexts += componentTestStepContext(dummyComponent) => [
						steps += testStep("wait").withReferenceToTemplateVariable("secs")
					]
				]
			]
		]
		macroModel.register("tml")

		val allEnvVars=envVariables("envVar", "myEnvString")
		val envVar = allEnvVars.head
		val myEnvString = allEnvVars.last
		val tclModel = tclModel => [
			environmentVariables.addAll(allEnvVars)
			test = testCase("MyTest") => [
				// use macro "mycall" using env param (no error, since type String is provided and String is expected)
				steps += specificationStep("test", "something") => [
					contexts += macroTestStepContext(macroModel.macroCollection) => [
						step = testStep("mycall").withReferenceToVariable(myEnvString)
					]
				]
				// use macro "othercall" using env param (error expected, since type String is provided and long is expected)
				steps += specificationStep("test", "other") => [
					contexts += macroTestStepContext(macroModel.macroCollection) => [
						step = testStep("othercall").withReferenceToVariable(envVar)
					]
				]
			]
		]
		tclModel.register('MyTest', 'tcl')

		val somethingContext = tclModel.test.steps.head.contexts.head as MacroTestStepContext
		val otherContext = tclModel.test.steps.last.contexts.head as MacroTestStepContext

		// when
		val setWithString = tclValidator.getAllTypeUsagesOfVariable(somethingContext, "myEnvString")
		val setWithLong = tclValidator.getAllTypeUsagesOfVariable(otherContext, "envVar")

		// then
		setWithString.assertSize(1)
		setWithString.head.simpleName.assertEquals(String.simpleName)

		setWithLong.assertSize(1)
		setWithLong.head.simpleName.assertEquals(long.simpleName)

		validator.assertError(tclModel, TEST_STEP, TclValidator.INVALID_TYPED_VAR_DEREF)
	}

	@Test
	def void testIndirectCallVariableTypeChecks() {
		// given
		val macroModel = tclModel
		macroModel => [
			macroCollection = macroCollection("MacroCollection") // is referenced => assignment must take place beforehand
			macroCollection => [
				// calls macro "othercall" with one parameter "unknown" (which is expected to be of type long)
				macros += macro("MyCallMacro") => [
					template = template("mycall").withParameter("unknown")
					contexts += macroTestStepContext(macroModel.macroCollection) => [
						step = testStep("othercall").withReferenceToTemplateVariable("unknown")
					]
				]
				macros += macro("OtherCallMacro") => [
					template = template("othercall").withParameter("secs")
					contexts += componentTestStepContext(dummyComponent) => [
						steps += testStep("wait").withReferenceToTemplateVariable("secs") // secs are expected to be of type long in aml fixture
					]
				]
			]
		]
		macroModel.register("tml")

		// call "mycall" with env parameter (which is of type String, transitively expected is type long) ...
		val tclModel = tclCallingMyCallMacroWithOneEnvParam("myEnvString", macroModel)

		val myCallContext = tclModel.test.steps.head.contexts.head

		// when
		val setWithLong = tclValidator.getAllTypeUsagesOfVariable(myCallContext, "myEnvString")

		// then
		setWithLong.assertSize(1)
		setWithLong.head.simpleName.assertEquals(long.simpleName)
		validator.assertError(tclModel, TEST_STEP, TclValidator.INVALID_TYPED_VAR_DEREF)
	}

	@Test
	def void testIndirectCallVariableWithMultipleUsageTypeChecks() {
		// given
		val macroModel = tclModel
		macroModel => [
			macroCollection = macroCollection("MacroCollection") // is referenced => assignment must take place beforehand
			macroCollection => [
				macros += otherCallMacroWithTwoParamsWithTypeLongAndStringRespectively
				// calls macro "othercall" with parameter "unknown" as first and second parameter (which are expected to be of type long and String)
				macros += macro("MyCallMacro") => [
					template = template("mycall").withParameter("unknown")
					contexts += macroTestStepContext(macroModel.macroCollection) => [
						step = testStep("othercall").withReferenceToTemplateVariable("unknown").withText("with").
							withReferenceToTemplateVariable("unknown")
					]
				]
			]
		]
		macroModel.register("tml")

		// since tcl calls mycall Macro with environment variable (which always has type String)
		// and this parameter is transitively used for calls in the aml expecting long and String ...
		val tclModel = tclCallingMyCallMacroWithOneEnvParam("myEnvString", macroModel)

		val myCallContext = tclModel.test.steps.head.contexts.head

		// when
		val setWithLong = tclValidator.getAllTypeUsagesOfVariable(myCallContext, "myEnvString")

		// then
		setWithLong.assertSize(2)
		setWithLong.map[simpleName].toList => [
			contains(long.simpleName) // one usage expects type long
			contains(String.simpleName) // one usage expects type String
		]
		validator.assertError(tclModel, TEST_STEP, TclValidator.INVALID_TYPED_VAR_DEREF) // since environment variables are of type String, report invalid usage
	}

	@Test
	def void testIndirectCallValidation() {
		// given + when
		val macroModel = tclModel
		macroModel => [
			macroCollection = macroCollection("MacroCollection") // is referenced => assignment must take place beforehand
			macroCollection => [
				macros += otherCallMacroWithTwoParamsWithTypeLongAndStringRespectively
				// calls macro "othercall" with parameter "3" and "unknown" (which will satisfy the expected types long and String)
				macros += macro("MyCallMacro") => [
					template = template("mycall").withParameter("unknown")
					contexts += macroTestStepContext(macroModel.macroCollection) => [
						step = testStep("othercall").withParameter("3").withText("with").
							withReferenceToTemplateVariable("unknown")
					]
				]
			]
		]
		macroModel.register("tml")
		// since tcl calls mycall Macro with environment variable (which always has type String)
		// and this parameter is transitively used for calls expecting type String ... (no errors expected)
		val tclModel = tclCallingMyCallMacroWithOneEnvParam("myEnvString", macroModel)

		// then
		validator.assertNoError(tclModel, TclValidator.INVALID_TYPED_VAR_DEREF)
		validator.assertNoError(tclModel, TclValidator.INVALID_VAR_DEREF)
	}
	
	@Test
	def void useAssignedVariableInParameterPosition() {
		// given
		val amlModel = amlTestModels.dummyComponent(resourceSet)
		amlModel.register(resourceSet, 'aml')
		val dummyComponent = amlModel.components.head
		
		val tclModel = tclModel => [
			test = testCase('Test') => [
				steps += specificationStep('spec') => [
					contexts += componentTestStepContext(dummyComponent) => [
						val assignment = testStepWithAssignment('variable', 'getValue').withElement("dummyElement") // get something of type string
						steps += assignment
						steps += testStep('start').withReferenceToVariable(assignment.variable) 
					]
				]
			]
		]
		tclModel.register('test', 'tcl')

		// when then
		validator.assertNoErrors(tclModel)
	}
	
	@Test
	def void useAssignedVariableInParameterPositionWrongOrder() {
		// given 
		val amlModel = amlTestModels.dummyComponent(resourceSet)
		amlModel.register(resourceSet, 'aml')
		val dummyComponent = amlModel.components.head
		
		val tclModel = tclModel => [
			test = testCase('Test') => [
				steps += specificationStep('spec') => [
					contexts += componentTestStepContext(dummyComponent) => [
						val assignment = testStepWithAssignment('variable', 'getValue').withElement("dummyElement") // assignment can be faulty, since checks on the type of 'variable' won't be carried out  
						steps += testStep('start').withReferenceToVariable(assignment.variable)
						steps += assignment
					]
				]
			]
		]
		tclModel.register('test', 'tcl')
		
		// when then
		validator.assertError(tclModel, TEST_STEP, TclValidator.INVALID_VAR_DEREF) // since assignment must take place before usage!
	}
	
	private def Macro otherCallMacroWithTwoParamsWithTypeLongAndStringRespectively() {
		return macro("OtherCallMacro") => [
			template = template("othercall").withParameter("secs").withText("with").withParameter("strParam")
			contexts += componentTestStepContext(dummyComponent) => [
				steps += testStep("wait").withReferenceToTemplateVariable("secs")
				steps += testStep("start").withReferenceToTemplateVariable("strParam")
			]
		]
	}

	private def TclModel tclCallingMyCallMacroWithOneEnvParam(String envVarString, TclModel tmlModel) {
		val envVar=envVariables(envVarString).head
		val tclModel = tclModel => [
			environmentVariables += envVar
			test = testCase("MyTest") => [
				steps += specificationStep("test", "something") => [
					contexts += macroTestStepContext(tmlModel.macroCollection) => [
						step = testStep("mycall").withReferenceToVariable(envVar)
					]
				]
			]
		]
		tclModel.register('MyTest', 'tcl')
		return tclModel
	}
}
