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
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.dsl.tests.TclModelGenerator
import org.testeditor.tcl.util.TclModelUtil

import static org.testeditor.tcl.TclPackage.Literals.*

class TclParameterUsageValidatorTest extends AbstractUnmockedTclValidatorTest {
	
	@Inject extension AmlModelGenerator
	@Inject extension TclModelGenerator
	@Inject TclModelUtil tclModelUtil


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
		macroModel.addToResourceSet("test.tml")

		val allEnvVars=environmentVariables("envVar", "myEnvString")
		val envVar = allEnvVars.head
		val myEnvString = allEnvVars.last
		val tclModel = tclModel => [
			environmentVariables.addAll(allEnvVars)
			test = testCase("MyTest") => [
				// use macro "mycall" using env param (no error, since type String is provided and String is expected)
				steps += specificationStep("test", "something") => [
					contexts += macroTestStepContext(macroModel.macroCollection) => [
						step = testStep("mycall").withReferenceToVariable(myEnvString)
						verifyVariableTypeUsage(myEnvString.name, #[String.simpleName]) // intermediate model check
					]
				]
				// use macro "othercall" using env param (error expected, since type String is provided and long is expected)
				steps += specificationStep("test", "other") => [
					contexts += macroTestStepContext(macroModel.macroCollection) => [
						step = testStep("othercall").withReferenceToVariable(envVar)
						verifyVariableTypeUsage(envVar.name, #[long.simpleName]) // intermediate model check
					]
				]
			]
		]
		tclModel.addToResourceSet('MyTest.tcl')
	
		// when validator is run, then
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
		macroModel.addToResourceSet("test.tml")

		// call "mycall" with env parameter (which is of type String, transitively expected is type long) ...
		val tclModel = tclCallingMyCallMacroWithOneEnvParam("myEnvString", macroModel, #[long.simpleName])

		// when validator is run, then
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
		macroModel.addToResourceSet("test.tml")

		// since tcl calls mycall Macro with environment variable (which always has type String)
		// and this parameter is transitively used for calls in the aml expecting long and String ...
		val tclModel = tclCallingMyCallMacroWithOneEnvParam("myEnvString", macroModel, #[long.simpleName, String.simpleName])

		// when validator is run, then
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
		macroModel.addToResourceSet("test.tml")
		// since tcl calls mycall Macro with environment variable (which always has type String)
		// and this parameter is transitively used for calls expecting type String ... (no errors expected)
		val tclModel = tclCallingMyCallMacroWithOneEnvParam("myEnvString", macroModel, #[String.simpleName])

		// then
		validator.assertNoError(tclModel, TclValidator.INVALID_TYPED_VAR_DEREF)
		validator.assertNoError(tclModel, TclValidator.INVALID_VAR_DEREF)
	}
	
	@Test
	def void useAssignedVariableInParameterPosition() {
		// given
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
		tclModel.addToResourceSet('test.tcl')

		// when then
		validator.assertNoErrors(tclModel)
	}
	
	@Test
	def void useAssignedVariableInParameterPositionWrongOrder() {
		// given 	
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
		tclModel.addToResourceSet('test.tcl')
		
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

	/** 
	 * create a test that calls "mycall" macro with the reference to an environment variable, validating that
	 * the parameter is used in positions that expect the passed types (simple names)
	 */
	private def TclModel tclCallingMyCallMacroWithOneEnvParam(String envVarString, TclModel tmlModel, Iterable<String> types) {
		val envVar=environmentVariables(envVarString).head
		val tclModel = tclModel => [
			environmentVariables += envVar
			test = testCase("MyTest") => [
				steps += specificationStep("test", "something") => [
					contexts += macroTestStepContext(tmlModel.macroCollection) => [
						step = testStep("mycall").withReferenceToVariable(envVar)
						verifyVariableTypeUsage(envVar.name, types)
					]
				]
			]
		]
		tclModel.addToResourceSet('MyTest.tcl')
		return tclModel
	}

	/**
	 * verify model to have the expected type usages when querying for a certain variable in a given context
	 */
	private def void verifyVariableTypeUsage(TestStepContext context, String variable, Iterable<String> types) {
		val typeSet = tclModelUtil.getAllTypeUsagesOfVariable(context, variable).map[simpleName].toSet
		typeSet.assertSize(types.size)
		types.forEach[ assertTrue(typeSet.contains(it)) ]
	}

}
