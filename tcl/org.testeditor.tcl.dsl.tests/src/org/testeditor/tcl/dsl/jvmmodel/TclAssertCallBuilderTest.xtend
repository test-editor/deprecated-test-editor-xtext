package org.testeditor.tcl.dsl.jvmmodel

import com.google.inject.Provider
import javax.inject.Inject
import org.eclipse.xtext.common.types.TypesFactory
import org.eclipse.xtext.resource.XtextResourceSet
import org.eclipse.xtext.xbase.jvmmodel.JvmTypeReferenceBuilder
import org.junit.Test
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.aml.dsl.tests.AmlModelGenerator
import org.testeditor.aml.impl.AmlFactoryImpl
import org.testeditor.tcl.dsl.tests.TclModelGenerator
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest
import org.testeditor.tml.AENullOrBoolCheck
import org.testeditor.tml.AEVariableReference
import org.testeditor.tml.util.TmlModelUtil

import static org.mockito.Matchers.*
import static org.mockito.Mockito.*

class TclAssertCallBuilderTest extends AbstractParserTest {

	@InjectMocks TclAssertCallBuilder assertCallBuilder // class under test
	@Mock TmlModelUtil tmlModelUtil // injected into class under test
	@Inject extension TclModelGenerator tclModelGenerator

	@Inject Provider<XtextResourceSet> resourceSetProvider
	@Inject AmlModelGenerator amlModelGenerator
	@Inject AmlFactoryImpl amlFactory
	@Inject TypesFactory typesFactory
	@Inject JvmTypeReferenceBuilder.Factory jvmTypeReferenceBuilderFactory

	@Test
	def void testEqualsGen() {
		// given
		val expression = aeComparison => [
			left = flatVarRef("variable")
			comparator = comparatorEquals
			right = aeStringConstant => [string = "test"]
		]
		// when
		val generatedCode = assertCallBuilder.build(expression)
		// then
		assertCodeLine('org.junit.Assert.assertEquals("", "test", variable);', generatedCode)
	}

	@Test
	def void testNotEqualsGen() {
		// given
		val expression = aeComparison => [
			left = flatVarRef("variable")
			comparator = comparatorNotEquals
			right = aeStringConstant => [string = "test"]
		]
		// when
		val generatedCode = assertCallBuilder.build(expression)
		// then
		assertCodeLine('org.junit.Assert.assertNotEquals("", "test", variable);', generatedCode)
	}

	@Test
	def void testNotNullGen() {
		// given
		val expression = nullOrBoolCheck("variable")
		// when
		val generatedCode = assertCallBuilder.build(expression)
		// then
		assertCodeLine('org.junit.Assert.assertNotNull("", variable);', generatedCode)
	}

	@Test
	def void testNullGen() {
		// given
		val expression = nullOrBoolCheck("variable") => [negated = true]
		// when
		val generatedCode = assertCallBuilder.build(expression)
		// then
		assertCodeLine('org.junit.Assert.assertNull("", variable);', generatedCode)
	}

	@Test
	def void testBooleanValue() {
		// given
		variableType = boolean
		val expression = nullOrBoolCheck("variable")
		// when
		val generatedCode = assertCallBuilder.build(expression)
		// then
		assertCodeLine('org.junit.Assert.assertTrue("", variable);', generatedCode)
	}

	@Test
	def void testNotBooleanValue() {
		// given
		variableType = boolean
		val expression = nullOrBoolCheck("variable") => [negated = true]
		// when
		val generatedCode = assertCallBuilder.build(expression)
		// then
		assertCodeLine('org.junit.Assert.assertFalse("", variable);', generatedCode)
	}

	@Test
	def void testBooleanObject() {
		// given
		variableType = Boolean
		val expression = nullOrBoolCheck("variable")
		// when
		val generatedCode = assertCallBuilder.build(expression)
		// then
		assertCodeLine('org.junit.Assert.assertTrue("", (variable != null) && variable.booleanValue());', generatedCode)
	}

	@Test
	def void testNotBooleanObject() {
		// given
		variableType = Boolean
		val expression = nullOrBoolCheck("variable") => [negated = true]
		// when
		val generatedCode = assertCallBuilder.build(expression)
		// then
		assertCodeLine('org.junit.Assert.assertFalse("", (variable != null) && variable.booleanValue());', generatedCode)
	}

	@Test
	def void testMatches() {
		// given
		val expression = aeComparison => [
			left = flatVarRef("variable")
			comparator = comparatorMatches
			right = aeStringConstant => [string = "ohoh"]
		]
		// when
		val generatedCode = assertCallBuilder.build(expression)
		// then
		assertCodeLine('org.junit.Assert.assertTrue("", variable.matches("ohoh"));', generatedCode)
	}

	@Test
	def void testDoesNotMatch() {
		// given
		val expression = aeComparison => [
			left = flatVarRef("variable")
			comparator = comparatorDoesNotMatch
			right = aeStringConstant => [string = "ohoh"]
		]
		// when
		val generatedCode = assertCallBuilder.build(expression)
		// then
		assertCodeLine('org.junit.Assert.assertFalse("", variable.matches("ohoh"));', generatedCode)
	}

	@Test
	def void testWithMapDereference() {
		// given
		val expression = aeComparison => [
			left = mapVarRef("variable", "key")
			comparator = comparatorEquals
			right = aeStringConstant => [string = "test"]
		]
		// when
		val generatedCode = assertCallBuilder.build(expression)
		// then
		assertCodeLine('org.junit.Assert.assertEquals("", "test", variable.get("key"));', generatedCode)
	}

	@Test
	def void testWithMapKeyAsString() {
		// given
		val expression = aeComparison => [
			left = mapVarRef("variable", "key with spaces")
			comparator = comparatorEquals
			right = aeStringConstant => [string = "test"]
		]
		// when
		val generatedCode = assertCallBuilder.build(expression)
		// then
		assertCodeLine('org.junit.Assert.assertEquals("", "test", variable.get("key with spaces"));', generatedCode)
	}

	/** make sure that questions to the type of the referenced variable within the assertion is set to clazz */
	private def void setVariableType(Class<?> clazz) {
		val jvmTypeReferenceBuilder = jvmTypeReferenceBuilderFactory.create(resourceSetProvider.get)
		val jvmType = jvmTypeReferenceBuilder.typeRef(clazz)

		when(tmlModelUtil.getInteraction(any)).thenReturn(
			amlModelGenerator.interactionType("myinteraction") => [
				template = amlModelGenerator.template("some")
				defaultMethod = amlFactory.createMethodReference => [
					operation = typesFactory.createJvmOperation => [
						returnType = jvmType
					]
				]
			]
		)
	}

	private def AEVariableReference flatVarRef(String variable) {
		aeVariableReference => [testStepWithAssignment = testStepWithAssignment(variable, "some")]
	}

	private def AEVariableReference mapVarRef(String variable, String myKey) {
		aeVariableReference => [
			testStepWithAssignment = testStepWithAssignment(variable, "some")
			key = myKey
		]
	}

	private def AENullOrBoolCheck nullOrBoolCheck(String variable) {
		aeNullOrBoolCheck => [
			varReference = aeVariableReference => [
				testStepWithAssignment = testStepWithAssignment(variable, "some")
			]
		]
	}

	// assert that the generated code holds 2 lines of which the second is identical to expectedCode
	private def void assertCodeLine(String expectedCode, String generatedCode) {
		val generatedCodeLines = generatedCode.split(System.lineSeparator)
		assertSize(generatedCodeLines, 2, "expecting two generated code lines, a comment and the assertion call")
		val assertMethod = generatedCodeLines.last
		assertMethod.assertEquals(expectedCode)
	}

}
