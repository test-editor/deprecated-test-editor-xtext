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

import com.google.inject.Provider
import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.resource.XtextResourceSet
import org.junit.Before
import org.junit.Test
import org.testeditor.aml.Component
import org.testeditor.aml.dsl.AmlStandaloneSetup
import org.testeditor.aml.dsl.tests.AmlModelGenerator
import org.testeditor.dsl.common.testing.DummyFixture
import org.testeditor.tcl.Macro
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.tests.TclModelGenerator
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest

import static org.testeditor.tcl.TclPackage.Literals.*

class TclParameterUsageValidatorTest extends AbstractParserTest {
	@Inject protected Provider<XtextResourceSet> resourceSetProvider
	@Inject protected XtextResourceSet resourceSet
	@Inject ValidationTestHelper validator

	var Component dummyComponent
	@Inject protected TclValidator tclValidator // class under test (not mocked)
	@Inject extension AmlModelGenerator amlModelGenerator
	@Inject protected extension TclModelGenerator tclModelGenerator

	@Before
	def void setup() {
		resourceSet = resourceSetProvider.get
		resourceSet.classpathURIContext = this
		new AmlStandaloneSetup().createInjectorAndDoEMFRegistration // needs to be registered to register aml models

		// build component "Dummy" with two interactions, "start" with a string parameter, "wait" with a long parameter
		val amlModel = amlModel => [
			withNamespaceImport("org.testeditor.dsl.common.testing")
			interactionTypes += interactionType("wait") => [
				template = template("wait").withParameter("secs")
				defaultMethod = methodReference(resourceSet, DummyFixture, "waitSeconds", "secs")
			]
			interactionTypes += interactionType("start") => [
				template = template("start").withParameter("appname")
				defaultMethod = methodReference(resourceSet, DummyFixture, "startApplication", "appname")
			]
		]
		val startInteraction = amlModel.interactionTypes.last
		val waitInteraction = amlModel.interactionTypes.head
		amlModel => [
			componentTypes += componentType("DummyCT") => [
				interactionTypes += startInteraction
				interactionTypes += waitInteraction
			]
		]

		val dummyCT = amlModel.componentTypes.head
		amlModel => [components += component("Dummy") => [type = dummyCT]]

		amlModel.register("aml")

		dummyComponent = amlModel.components.head
	}

	@Test
	def void testDirectCallVariableTypeChecks() {
		val macroModel = tclModel("MacroCollection") => [
			macroCollection = macroCollection() => [
				// macro calls (directly) the aml interaction "start" (which expects the parameter to be of type String)
				macros += macro("MyCallMacro") => [
					template = template("mycall").withParameter("appname")
					contexts += componentTestStepContext(dummyComponent) => [
						steps += testStep("start").withVariableReference("appname")
					]
				]
				// macro calls (directly) the aml interaction "wait" (which expects the parameter to be of type long)
				macros += macro("OtherCallMacro") => [
					template = template("othercall").withParameter("secs")
					contexts += componentTestStepContext(dummyComponent) => [
						steps += testStep("wait").withVariableReference("secs")
					]
				]
			]
		]
		macroModel.register("tml")

		val tclModel = tclModel("MyTest") => [
			environmentVariableReferences += envVariables("envVar", "myEnvString")
			test = testCase => [
				// use macro "mycall" using env param (no error, since type String is provided and String is expected)
				steps += specificationStep("test", "something") => [
					contexts += macroTestStepContext(macroModel.macroCollection) => [
						step = testStep("mycall").withVariableReference("myEnvString")
					]
				]
				// use macro "othercall" using env param (error expected, since type String is provided and long is expected)
				steps += specificationStep("test", "other") => [
					contexts += macroTestStepContext(macroModel.macroCollection) => [
						step = testStep("othercall").withVariableReference("envVar")
					]
				]
			]
		]
		tclModel.register("tcl")

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
		val macroModel = tclModel("MacroCollection")
		macroModel => [
			macroCollection = macroCollection() // is referenced => assignment must take place beforehand
			macroCollection => [
				// calls macro "othercall" with one parameter "unknown" (which is expected to be of type long)
				macros += macro("MyCallMacro") => [
					template = template("mycall").withParameter("unknown")
					contexts += macroTestStepContext(macroModel.macroCollection) => [
						step = testStep("othercall").withVariableReference("unknown")
					]
				]
				macros += macro("OtherCallMacro") => [
					template = template("othercall").withParameter("secs")
					contexts += componentTestStepContext(dummyComponent) => [
						steps += testStep("wait").withVariableReference("secs") // secs are expected to be of type long in aml fixture
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
		val macroModel = tclModel("MacroCollection")
		macroModel => [
			macroCollection = macroCollection() // is referenced => assignment must take place beforehand
			macroCollection => [
				macros += otherCallMacroWithTwoParamsWithTypeLongAndStringRespectively
				// calls macro "othercall" with parameter "unknown" as first and second parameter (which are expected to be of type long and String)
				macros += macro("MyCallMacro") => [
					template = template("mycall").withParameter("unknown")
					contexts += macroTestStepContext(macroModel.macroCollection) => [
						step = testStep("othercall").withVariableReference("unknown").withText("with").
							withVariableReference("unknown")
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
		val macroModel = tclModel("MacroCollection")
		macroModel => [
			macroCollection = macroCollection() // is referenced => assignment must take place beforehand
			macroCollection => [
				macros += otherCallMacroWithTwoParamsWithTypeLongAndStringRespectively
				// calls macro "othercall" with parameter "3" and "unknown" (which will satisfy the expected types long and String)
				macros += macro("MyCallMacro") => [
					template = template("mycall").withParameter("unknown")
					contexts += macroTestStepContext(macroModel.macroCollection) => [
						step = testStep("othercall").withParameter("3").withText("with").
							withVariableReference("unknown")
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

	private def Macro otherCallMacroWithTwoParamsWithTypeLongAndStringRespectively() {
		return macro("OtherCallMacro") => [
			template = template("othercall").withParameter("secs").withText("with").withParameter("strParam")
			contexts += componentTestStepContext(dummyComponent) => [
				steps += testStep("wait").withVariableReference("secs")
				steps += testStep("start").withVariableReference("strParam")
			]
		]
	}

	private def TclModel tclCallingMyCallMacroWithOneEnvParam(String envVar, TclModel tmlModel) {
		val tclModel = tclModel("MyTest") => [
			environmentVariableReferences += envVariables(envVar)
			test = testCase => [
				steps += specificationStep("test", "something") => [
					contexts += macroTestStepContext(tmlModel.macroCollection) => [
						step = testStep("mycall").withVariableReference(envVar)
					]
				]
			]
		]
		tclModel.register("tcl")
		return tclModel
	}

	/** 
	 * register the given model with the resource set (for cross linking)
	 */
	private def <T extends EObject> T register(T model, String fileExtension) {
		model.register(resourceSet, fileExtension)
	}

}
