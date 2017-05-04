package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.common.types.JvmType
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable
import org.junit.Before
import org.junit.Test
import org.mockito.Mock
import org.testeditor.aml.AmlModel
import org.testeditor.dsl.common.testing.DummyFixture
import org.testeditor.tcl.Expression
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestStepWithAssignment
import org.testeditor.tcl.dsl.tests.TclModelGenerator

import static org.mockito.Matchers.*
import static org.mockito.Mockito.*

class TclMapAssignmentGenerationTest extends AbstractTclGeneratorIntegrationTest {

	@Inject TclJvmModelInferrer jvmModelInferrer // class under test
	@Mock ITreeAppendable outputStub

	@Inject extension TclModelGenerator
	
	var AmlModel amlModel

	@Before
	def void initMocks() {
		when(outputStub.trace(any(EObject))).thenReturn(outputStub)
		when(outputStub.append(any(CharSequence))).thenReturn(outputStub)
		when(outputStub.append(any(JvmType))).thenReturn(outputStub)
		when(outputStub.newLine).thenReturn(outputStub)
	}
	@Before
	def void parseDummyAmlModel() {
		amlModel = DummyFixture.amlModel.parseAml
	}

	@Test
	def void testMapAssignmentWithStringValue() {
		// given
		val expression = jsonString("value")
		val tclModel = createMapEntryAssignmentModel("mapVariable", null, expression)

		// when
		jvmModelInferrer.generateMethodBody(tclModel.test, outputStub)

		// then
		verify(outputStub).append('mapVariable.put("key", "value");')
	}
	
	@Test
	def void testMapAssignmentWithVariableReference() {
		// given
		val otherMapAssignment = testStepWithAssignment("otherMap", "Read", "map", "from").withElement("bar")
		val expression = variableReferencePathAccess => [
			variable = otherMapAssignment.variable
			path += keyPathElement => [ key = "some key with spaces" ]
		]
		val tclModel = createMapEntryAssignmentModel("mapVariable", otherMapAssignment, expression)
		
		// when
		jvmModelInferrer.generateMethodBody(tclModel.test, outputStub)

		// then
		verify(outputStub).append('mapVariable.put("key", otherMap.get("some key with spaces"));')
	}
	

	@Test
	def void testMapAssignmentWithBoolVariableReference() {
		// given
		val boolAssignment = testStepWithAssignment("myBool", "Read", "bool", "from").withElement("bar")
		val expression = variableReference => [variable = boolAssignment.variable]
		val tclModel = createMapEntryAssignmentModel("mapVariable", boolAssignment, expression)

		// when
		jvmModelInferrer.generateMethodBody(tclModel.test, outputStub)

		// then
		verify(outputStub).append('mapVariable.put("key", String.valueOf(myBool));')
	}

	@Test
	def void testMapAssignmentWithLongVariableReference() {
		// given
		val longAssignment = testStepWithAssignment("myLong", "Read", "long", "from").withElement("bar")
		val expression = variableReference => [variable = longAssignment.variable]
		val tclModel = createMapEntryAssignmentModel("mapVariable", longAssignment, expression)

		// when
		jvmModelInferrer.generateMethodBody(tclModel.test, outputStub)

		// then
		verify(outputStub).append('mapVariable.put("key", String.valueOf(myLong));')
	}

	/**
	 * generate a tcl model looking something like:<br/>
	 * <pre>
	 * {@code
	 * package com.example
	 * * my test
	 *    Component: GreetingApplication
	 *       «additionalVarThroughAssignmentStep»
	 *       - «mapVariable» = Read map from <bar>
	 *       - @«mapVariable».key = «expression»
	 * }
	 * </pre>
	 */
	private def TclModel createMapEntryAssignmentModel(String mapVariable, TestStepWithAssignment additionalVarThroughAssignmentStep, Expression expression) {
		val dummyComponent = amlModel.components.findFirst[name=="GreetingApplication"]
		val tclModel = tclModel.withImport("com.example") => [
			it.test = testCase => [
				it.steps += specificationStep("my", "test") => [
					contexts += componentTestStepContext(dummyComponent) => [
						if (additionalVarThroughAssignmentStep !== null) {
							steps += additionalVarThroughAssignmentStep
						}
						val mapAssignment = testStepWithAssignment(mapVariable, "Read", "map", "from").withElement("bar")
						steps += mapAssignment
						steps += assignmentThroughPath(mapAssignment.variable, "key") => [
							it.expression = expression 
						]
					]
				]
			]
		]
		return tclModel
	}
	
}
