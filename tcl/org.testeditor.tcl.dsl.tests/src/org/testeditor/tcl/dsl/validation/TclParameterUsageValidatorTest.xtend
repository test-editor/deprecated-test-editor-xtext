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
import java.util.UUID
import javax.inject.Inject
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.resource.XtextResourceSet
import org.junit.Before
import org.junit.Test
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Component
import org.testeditor.aml.dsl.AmlStandaloneSetup
import org.testeditor.aml.dsl.tests.AmlModelGenerator
import org.testeditor.dsl.common.testing.DummyFixture
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.tests.TclModelGenerator
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest
import org.testeditor.tml.Macro
import org.testeditor.tml.TmlModel
import org.testeditor.tml.dsl.TmlStandaloneSetup

import static org.testeditor.tml.TmlPackage.Literals.*

class TclParameterUsageValidatorTest extends AbstractParserTest {
	@Inject TclValidator tclValidator // class under test
	@Inject protected Provider<XtextResourceSet> resourceSetProvider
	@Inject protected XtextResourceSet resourceSet
	@Inject ValidationTestHelper validator

	protected ParseHelper<AmlModel> amlParseHelper
	protected ParseHelper<TmlModel> tmlParseHelper

	var Component dummyComponent

	@Inject extension TclModelGenerator
	@Inject extension AmlModelGenerator

	@Before
	def void setup() {
		resourceSet = resourceSetProvider.get
		resourceSet.classpathURIContext = this
		val injector = (new AmlStandaloneSetup).createInjectorAndDoEMFRegistration
		amlParseHelper = injector.getInstance(ParseHelper)
		val tmlInjector = (new TmlStandaloneSetup).createInjectorAndDoEMFRegistration
		tmlParseHelper = tmlInjector.getInstance(ParseHelper)

		// build component "Dummy" with two interactions, "start" with a string parameter, "wait" with a long parameter
		val newAmlModel = amlModel.withPackage("org.test").withNamespaceImport("org.testeditor.dsl.common.testing").
			withInteractionType(
				interactionType("wait").withTemplate(template("wait").withParameter("secs")).withMethodReference(
					methodReference(resourceSet, DummyFixture, "waitSeconds", "secs"))
			).withInteractionType(
				interactionType("start").withTemplate(template("start").withParameter("appname")).withMethodReference(
					methodReference(resourceSet, DummyFixture, "startApplication", "appname"))
			)

		val startInteraction = newAmlModel.interactionTypes.last
		val waitInteraction = newAmlModel.interactionTypes.head
		newAmlModel.withComponentType(componentType("DummyCT").withInteractions(startInteraction, waitInteraction))

		val dummyCT = newAmlModel.componentTypes.head
		newAmlModel.withComponent(component("Dummy").isA(dummyCT)) //
		newAmlModel.register("aml")

		dummyComponent = newAmlModel.components.head
	}

	@Test
	def void testDirectCallVariableTypeChecks() {
		val tmlModel = tmlModel("MacroCollection").withPackage("org.test") //
			// macro calls (directly) the aml interaction "start" (which expects the parameter to be of type String)
			.withMacro(
				macro("MyCallMacro") //
				.withTemplate(template("mycall").withParameter("appname")) //
				.withTestStepContext(
					componentTestStepContext(dummyComponent).withTestStep(
						testStep("start").withVariableReference("appname")))) //
			// macro calls (directly) the aml interaction "wait" (which expects the parameter to be of type long)
			.withMacro(
				macro("OtherCallMacro") //
				.withTemplate(template("othercall").withParameter("secs")) //
				.withTestStepContext(
					componentTestStepContext(dummyComponent).withTestStep(
						testStep("wait").withVariableReference("secs")))).register("tml")
	
		val tclModel = tclModel.withPackage("org.test").withEnvVariable("envVar", "myEnvString").withTestCase(
			testCase("MyTest") //
			// use macro "mycall" using env param (no error, since type String is provided and String is expected)
			.withSpecificationStep(
				specificationStep("test", "something").withTestStepContext(
					macroTestStepContext(tmlModel).withTestStep(
						testStep("mycall").withVariableReference("myEnvString")))) //
			// use macro "othercall" using env param (error expected, since type String is provided and long is expected)
			.withSpecificationStep(
				specificationStep("test", "other").withTestStepContext(
					macroTestStepContext(tmlModel).withTestStep(
						testStep("othercall").withVariableReference("envVar"))))).register("tcl")

		val somethingContext = tclModel.test.steps.head.contexts.head
		val otherContext = tclModel.test.steps.last.contexts.head

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
		val tmlModel = tmlModel("MacroCollection").withPackage("org.test")
		tmlModel.withMacro( //
		// calls macro "othercall" with one parameter "unknown" (which is expected to be of type long)
		macro("MyCallMacro") //
		.withTemplate(template("mycall").withParameter("unknown")) //
		.withTestStepContext(
			macroTestStepContext(tmlModel).withTestStep(
				testStep("othercall").withVariableReference("unknown")))).withMacro(
				macro("OtherCallMacro") //
				.withTemplate(template("othercall").withParameter("secs")) //
				.withTestStepContext(
					componentTestStepContext(dummyComponent).withTestStep(
						testStep("wait").withVariableReference("secs")))) // secs are expected to be of type long in aml fixture
		.register("tml")

		// call "mycall" with env parameter (which is of type String, transitively expected is type long) ...
		val tclModel = tclCallingMyCallMacroWithOneEnvParam("myEnvString", tmlModel)

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
		val tmlModel = tmlModel("MacroCollection").withPackage("org.test")

		tmlModel.withMacro(otherCallMacroWithTwoParamsWithTypeLongAndStringRespectively) //
		// calls macro "othercall" with parameter "unknown" as first and second parameter (which are expected to be of type long and String)
		.withMacro(
			macro("MyCallMacro") //
			.withTemplate(template("mycall").withParameter("unknown")) //
			.withTestStepContext(
				macroTestStepContext(tmlModel).withTestStep(
					testStep("othercall").withVariableReference("unknown") //
					.withText("with").withVariableReference("unknown")))).register("tml")

		// since tcl calls mycall Macro with environment variable (which always has type String)
		// and this parameter is transitively used for calls in the aml expecting long and String ...
		val tclModel = tclCallingMyCallMacroWithOneEnvParam("myEnvString", tmlModel)

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
		val tmlModel = tmlModel("MacroCollection").withPackage("org.test")

		tmlModel.withMacro(otherCallMacroWithTwoParamsWithTypeLongAndStringRespectively) //
		// calls macro "othercall" with parameter "3" and "unknown" (which will satisfy the expected types long and String)
		.withMacro(
			macro("MyCallMacro") //
			.withTemplate(template("mycall").withParameter("unknown")) //
			.withTestStepContext(
				macroTestStepContext(tmlModel).withTestStep(testStep("othercall").withParameter("3") //
				.withText("with").withVariableReference("unknown")))).register("tml")

		// since tcl calls mycall Macro with environment variable (which always has type String)
		// and this parameter is transitively used for calls expecting type String ... (no errors expected)
		val tclModel = tclCallingMyCallMacroWithOneEnvParam("myEnvString", tmlModel)

		// then
		validator.assertNoError(tclModel, TclValidator.INVALID_TYPED_VAR_DEREF)
		validator.assertNoError(tclModel, TclValidator.INVALID_VAR_DEREF)
	}

	private def Macro otherCallMacroWithTwoParamsWithTypeLongAndStringRespectively() {
		return macro("OtherCallMacro") //
		.withTemplate(
			template("othercall").withParameter("secs").withText("with").withParameter("strParam")) //
		.withTestStepContext(componentTestStepContext(dummyComponent) //
		.withTestStep(testStep("wait").withVariableReference("secs")) //
		.withTestStep(testStep("start").withVariableReference("strParam")))
	}

	private def TclModel tclCallingMyCallMacroWithOneEnvParam(String envVar, TmlModel tmlModel) {
		val tclModel = tclModel.withPackage("org.test").withEnvVariable(envVar).withTestCase(
			testCase("MyTest").withSpecificationStep(
				specificationStep("test", "something").withTestStepContext(
					macroTestStepContext(tmlModel).withTestStep(
						testStep("mycall").withVariableReference(envVar)))))

		tclModel.register("tcl")
		return tclModel
	}

	/** 
	 * register the given model with the resource set (for cross linking)
	 */
	private def <T extends EObject> T register(T model, String fileExtension) {
		val uri = URI.createURI(UUID.randomUUID.toString + "." + fileExtension)

		val newResource = resourceSet.createResource(uri)
		newResource.getContents().add(model)
		return model
	}

}
