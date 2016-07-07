package org.testeditor.tcl.dsl.tests

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable
import org.junit.Before
import org.junit.Test
import org.mockito.Mock
import org.testeditor.aml.dsl.tests.AmlModelGenerator
import org.testeditor.dsl.common.testing.DummyFixture
import org.testeditor.tcl.dsl.jvmmodel.AbstractTclGeneratorIntegrationTest
import org.testeditor.tcl.dsl.jvmmodel.TclJvmModelInferrer

import static org.mockito.Matchers.*
import static org.mockito.Mockito.*

class TclCallParameterTest extends AbstractTclGeneratorIntegrationTest {

	@Inject TclJvmModelInferrer jvmModelInferrer // class under test
	@Mock ITreeAppendable outputStub

	@Inject extension AmlModelGenerator
	@Inject extension TclModelGenerator

	@Before
	def void initMocks() {
		when(outputStub.trace(any(EObject))).thenReturn(outputStub)
		when(outputStub.append(any(CharSequence))).thenReturn(outputStub)
	}

	@Test
	def void testCallParameterEscaping() {
		// given
		val amlModel = amlModel => [
			withNamespaceImport('org.testeditor.dsl.common.testing.*')
			val startInteraction = interactionType("start") => [
				defaultMethod = methodReference(resourceSet, DummyFixture, "startApplication", "appname")
				template = template("start").withParameter(defaultMethod.parameters.head)
			]
			interactionTypes += startInteraction

			val dummyComponentType = componentType("DummyCT") => [
				interactionTypes += startInteraction
			]
			componentTypes += dummyComponentType

			components += component("Dummy") => [type = dummyComponentType]
		]
		amlModel.addToResourceSet
		val dummyComponent = amlModel.components.head
		val tclModel = tclModel => [
			it.test = testCase("Test") => [
				it.steps += specificationStep("my", "test") => [
					contexts += componentTestStepContext(dummyComponent) => [
						steps += testStep('start').withParameter('te\\st\'')
					]
				]
			]
		]
		tclModel.addToResourceSet

		// when
		jvmModelInferrer.generateMethodBody(tclModel.test, outputStub, #{})

		// then
		// expectation is string is escaped properly
		verify(outputStub).append('dummyFixture.startApplication("te\\\\st\'");')
	}

}
