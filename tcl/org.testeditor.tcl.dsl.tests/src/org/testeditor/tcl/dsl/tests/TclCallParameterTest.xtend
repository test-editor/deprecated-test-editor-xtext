package org.testeditor.tcl.dsl.tests

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable
import org.junit.Before
import org.junit.Test
import org.mockito.Mock
import org.testeditor.aml.dsl.tests.common.AmlTestModels
import org.testeditor.tcl.dsl.jvmmodel.AbstractTclGeneratorIntegrationTest
import org.testeditor.tcl.dsl.jvmmodel.TclJvmModelInferrer

import static org.mockito.Matchers.*
import static org.mockito.Mockito.*

class TclCallParameterTest extends AbstractTclGeneratorIntegrationTest {

	@Inject TclJvmModelInferrer jvmModelInferrer // class under test
	@Mock ITreeAppendable outputStub

	@Inject extension TclModelGenerator
	@Inject AmlTestModels amlTestModels

	@Before
	def void initMocks() {
		when(outputStub.trace(any(EObject))).thenReturn(outputStub)
		when(outputStub.append(any(CharSequence))).thenReturn(outputStub)
	}

	@Test
	def void testCallParameterEscaping() {
		// given
		val amlModel = amlTestModels.dummyComponent(resourceSet)
		amlModel.addToResourceSet
		val dummyComponent = amlModel.components.head
		val tclModel = tclModel => [
			it.test = testCase => [
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
	
	@Test
	def void testAssignedVariableAsParameter() {
		// given
		val amlModel = amlTestModels.dummyComponent(resourceSet)
		amlModel.addToResourceSet
		val dummyComponent = amlModel.components.head
		
		val tclModel = tclModel => [
			test = testCase("Test") => [
				steps += specificationStep("spec") => [
					contexts += componentTestStepContext(dummyComponent) => [
						val assignment = testStepWithAssignment("variable", "getValue") // get something of type string
						steps += assignment
						steps += testStep('start').withReferenceToAssignmentVariable(assignment.variable) 
					]
				]
			]
		]
		tclModel.addToResourceSet

		// when
		jvmModelInferrer.generateMethodBody(tclModel.test, outputStub, #{})

		// then
		// expectation is string is escaped properly
		verify(outputStub).append('java.lang.String variable = ')
		verify(outputStub).append('dummyFixture.getValue();')
		verify(outputStub).append('dummyFixture.startApplication(variable);')
	}

}
