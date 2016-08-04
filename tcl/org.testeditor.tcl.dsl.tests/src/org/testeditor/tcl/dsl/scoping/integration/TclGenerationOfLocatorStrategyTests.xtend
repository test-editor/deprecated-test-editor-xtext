package org.testeditor.tcl.dsl.scoping.integration

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable
import org.junit.Before
import org.junit.Test
import org.mockito.Mock
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Component
import org.testeditor.aml.dsl.tests.AmlModelGenerator
import org.testeditor.dsl.common.testing.DummyFixture
import org.testeditor.dsl.common.testing.DummyLocatorStrategy
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.jvmmodel.AbstractTclGeneratorIntegrationTest
import org.testeditor.tcl.dsl.jvmmodel.TclJvmModelInferrer
import org.testeditor.tcl.dsl.tests.TclModelGenerator

import static org.mockito.Matchers.*
import static org.mockito.Mockito.*
import org.testeditor.dsl.common.testing.ResourceSetHelper

class TclGenerationOfLocatorStrategyTests extends AbstractTclGeneratorIntegrationTest {

	@Inject TclJvmModelInferrer jvmModelInferrer // class under test
	@Mock ITreeAppendable outputStub

	@Inject extension TclModelGenerator
	@Inject extension AmlModelGenerator
	@Inject extension ResourceSetHelper	

	@Before
	def void setUp() {
		when(outputStub.trace(any(EObject))).thenReturn(outputStub)
		when(outputStub.append(any(CharSequence))).thenReturn(outputStub)
	}

	def TclModel tclClickOnDummyButton(Component dummyComponent) {
		return tclModel => [
			test = testCase => [
				steps += specificationStep("my", "test") => [
					contexts += componentTestStepContext(dummyComponent) => [
						steps += testStep("click", "on").withElement("DummyButton")
					]
				]
			]
		]
	}

	/**
	 * generate aml that has an interaction with a defaultLocatorStrategy (if not null),
	 *   an element with locator elementLocator and with a locatorStrategy elementLocatorStrategy (if not null).
	 * 
	 * if no defaultLocatorStrategy is given, it is mandatory to have an elementLocatorStrategy
	 * if no elementLocatorStrategy is given, it is mandatory to have a defaultLocatoryStrategy
	 * if both strategies are given, the elementLocatorStrategy overrules the defaultLocatorStrategy
	 */
	def AmlModel amlWithDummyButtonAndLocatorStrategies(String defaultLocatorStrategy, String elementLocator,
		String elementLocatorStrategy) {
		return amlModel => [
			withTypeImport(resourceSet, "org.testeditor.dsl.common.testing.DummyLocatorStrategy")
			withTypeImport(resourceSet, "org.testeditor.dsl.common.testing.DummyFixture")

			val clickInteraction = interactionType("click") => [
				defaultMethod = methodReference(resourceSet, DummyFixture, "clickOn", "element").withLocatorStrategy
				template = template("click", "on").withParameter(defaultMethod.parameters.head)
				if (defaultLocatorStrategy !== null) {
					locatorStrategy = locatorStrategy(resourceSet, DummyLocatorStrategy, defaultLocatorStrategy)
				}
			]
			interactionTypes += clickInteraction

			val buttonType = componentElementType("Button") => [
				interactionTypes += clickInteraction
			]
			componentElementTypes += buttonType

			val dummyCT = componentType("DummyCT")
			componentTypes += dummyCT

			components += component("Dummy") => [
				type = dummyCT
				elements += componentElement("DummyButton") => [
					type = buttonType
					locator = elementLocator
					if (elementLocatorStrategy !== null) {
						locatorStrategy = locatorStrategy(resourceSet, DummyLocatorStrategy, elementLocatorStrategy)
					}
				]
			]
		]
	}

	@Test
	def void testDefaultLocatorStrategyGeneration() {
		// given
		val amlModel = amlWithDummyButtonAndLocatorStrategies(DummyLocatorStrategy.LABEL.name, "ok", null)
		amlModel.addToResourceSet

		val tcl = tclClickOnDummyButton(amlModel.components.head)

		// when
		jvmModelInferrer.generateMethodBody(tcl.test, outputStub, #{})

		// then
		verify(outputStub).append(
			'dummyFixture.clickOn("ok", org.testeditor.dsl.common.testing.DummyLocatorStrategy.LABEL);')
	}

	@Test
	def void testOverrideDefaultLocatorStrategyGeneration() {
		// given
		val amlModel = amlWithDummyButtonAndLocatorStrategies(DummyLocatorStrategy.SINGLE.name, "OK_BUTTON_ID",
			DummyLocatorStrategy.ID.name)
		amlModel.addToResourceSet

		val tcl = tclClickOnDummyButton(amlModel.components.head)

		// when
		jvmModelInferrer.generateMethodBody(tcl.test, outputStub, #{})

		// then
		verify(outputStub).append(
			'dummyFixture.clickOn("OK_BUTTON_ID", org.testeditor.dsl.common.testing.DummyLocatorStrategy.ID);')
	}

	@Test
	def void testWithoutDefaultLocatorStrategyGeneration() {
		// given
		val amlModel = amlWithDummyButtonAndLocatorStrategies(null, "OK_BUTTON_ID", DummyLocatorStrategy.ID.name)
		amlModel.addToResourceSet

		val tcl = tclClickOnDummyButton(amlModel.components.head)

		// when
		jvmModelInferrer.generateMethodBody(tcl.test, outputStub, #{})

		// then
		verify(outputStub).append(
			'dummyFixture.clickOn("OK_BUTTON_ID", org.testeditor.dsl.common.testing.DummyLocatorStrategy.ID);')
	}

}
