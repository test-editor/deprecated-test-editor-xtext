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
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestStepWithAssignment

import static org.mockito.Matchers.*
import static org.mockito.Mockito.*

class TclJsonAssignmentGenerationIntegrationTest extends AbstractTclGeneratorIntegrationTest {

	@Inject TclJvmModelInferrer jvmModelInferrer // class under test
	@Mock ITreeAppendable outputStub

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
	def void testJsonAssignmentWithStringValue() {
		// given
		val tclModel = createJsonEntryAssignmentModel("jsonVariable", null, "{ \"kk\" : \"jj\" }", "key 1", "key 2", "3.key")

		// when
		jvmModelInferrer.generateMethodBody(tclModel.test, outputStub)

		// then
		verify(outputStub).append('jsonVariable.getAsJsonObject().get("key 1").getAsJsonObject().get("key 2").getAsJsonObject().add("3.key", new com.google.gson.JsonParser().parse("{ \\"kk\\" : \\"jj\\" }"));')
	}
	
//	@Test
//	def void testMapAssignmentWithVariableReference() {
//		// given
//		val otherMapAssignment = testStepWithAssignment("otherMap", "Read", "map", "from").withElement("bar")
//		val expression = variableReferenceMapAccess => [
//			variable = otherMapAssignment.variable
//			key = "some key with spaces"
//		]
//		val tclModel = createJsonEntryAssignmentModel("mapVariable", otherMapAssignment, expression)
//		
//		// when
//		jvmModelInferrer.generateMethodBody(tclModel.test, outputStub)
//
//		// then
//		verify(outputStub).append('mapVariable.put("key", otherMap.get("some key with spaces"));')
//	}
//	
//
//	@Test
//	def void testMapAssignmentWithBoolVariableReference() {
//		// given
//		val boolAssignment = testStepWithAssignment("myBool", "Read", "bool", "from").withElement("bar")
//		val expression = variableReference => [variable = boolAssignment.variable]
//		val tclModel = createJsonEntryAssignmentModel("mapVariable", boolAssignment, expression)
//
//		// when
//		jvmModelInferrer.generateMethodBody(tclModel.test, outputStub)
//
//		// then
//		verify(outputStub).append('mapVariable.put("key", String.valueOf(myBool));')
//	}
//
//	@Test
//	def void testMapAssignmentWithLongVariableReference() {
//		// given
//		val longAssignment = testStepWithAssignment("myLong", "Read", "long", "from").withElement("bar")
//		val expression = variableReference => [variable = longAssignment.variable]
//		val tclModel = createJsonEntryAssignmentModel("mapVariable", longAssignment, expression)
//
//		// when
//		jvmModelInferrer.generateMethodBody(tclModel.test, outputStub)
//
//		// then
//		verify(outputStub).append('mapVariable.put("key", String.valueOf(myLong));')
//	}

	/**
	 * generate a tcl model looking something like:<br/>
	 * <pre>
	 * {@code
	 * package com.example
	 * * my test
	 *    Component: GreetingApplication
	 *       «additionalVarThroughAssignmentStep»
	 *       - «jsonVariable» = Read jsonObject from <bar>
	 *       - @«jsonVariable».key1.key2...keyN = «expression»
	 * }
	 * </pre>
	 */
	private def TclModel createJsonEntryAssignmentModel(String jsonVariable, TestStepWithAssignment additionalVarThroughAssignmentStep, String expressionString, String ... path) {
		val tclModel = '''
			package com.example
			
			# Test
			* some
			Component: GreetingApplication
			- jsonVariable = Read jsonObject from <bar>
			- jsonVariable.«path.map['''"«it»"'''].join('.')» = «expressionString»
		'''.toString.parseTcl
		
		return tclModel
		
	}
	
}
