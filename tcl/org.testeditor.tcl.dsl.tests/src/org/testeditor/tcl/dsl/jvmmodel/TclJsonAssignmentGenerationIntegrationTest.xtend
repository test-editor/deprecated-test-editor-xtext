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

	@Before
	def void initJvmTypeReferenceUtil() {
		jvmModelInferrer.initWith(null)
	}

	@Test
	def void testJsonAssignmentWithStringValue() {
		// given
		val tclModel = createJsonEntryAssignmentModel("", "{ \"kk\" : \"jj\" }", "key 1", "key 2", "3.key")

		// when
		jvmModelInferrer.generateMethodBody(tclModel.test, outputStub)

		// then
		verify(outputStub).append('jsonVariable.getAsJsonObject().get("key 1").getAsJsonObject().get("key 2").getAsJsonObject().add("3.key", new com.google.gson.JsonParser().parse("{ \\"kk\\" : \\"jj\\" }"));')
	}

	@Test
	def void testJsonObjectAssignmentWithVariableReference() {
		// given
		val tclModel = createJsonEntryAssignmentModel("- otherJsonVariable = Read jsonObject from <bar>",
			"otherJsonVariable.\"some key with spaces\"", "key")

		// when
		jvmModelInferrer.generateMethodBody(tclModel.test, outputStub)

		// then
		verify(outputStub).append('jsonVariable.getAsJsonObject().add("key", otherJsonVariable.getAsJsonObject().get("some key with spaces"));')
	}

	@Test
	def void testJsonObjectAssignmentWithBoolVariableReference() {
		// given
		val tclModel = createJsonEntryAssignmentModel("- myBool = Read bool from <bar>", "myBool", "key")

		// when
		jvmModelInferrer.generateMethodBody(tclModel.test, outputStub)

		// then
		verify(outputStub).append('jsonVariable.getAsJsonObject().add("key", new com.google.gson.JsonParser().parse(Boolean.toString(myBool)));')
	}

	@Test
	def void testJsonObjectAssignmentWithLongVariableReference() {
		// given
		val tclModel = createJsonEntryAssignmentModel("- myLong = Read long from <bar>", "myLong", "key")

		// when
		jvmModelInferrer.generateMethodBody(tclModel.test, outputStub)

		// then
		verify(outputStub).append('jsonVariable.getAsJsonObject().add("key", new com.google.gson.JsonParser().parse(Long.toString(myLong)));')
	}

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
	private def TclModel createJsonEntryAssignmentModel(String additionalTestStep, String expressionString,
		String ... path) {
		val tclModel = '''
			package com.example
			
			# Test
			* some
			Component: GreetingApplication
			- jsonVariable = Read jsonObject from <bar>
			«additionalTestStep»
			- jsonVariable.«path.map['''"«it»"'''].join('.')» = «expressionString»
		'''.toString.parseTcl

		return tclModel

	}

}
