package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.eclipse.xtext.xbase.jvmmodel.JvmTypeReferenceBuilder
import org.junit.Before
import org.junit.Test
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.aml.ModelUtil
import org.testeditor.tcl.StringConstant
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.VariableReferenceMapAccess
import org.testeditor.tcl.dsl.tests.TclModelGenerator
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest
import org.testeditor.tcl.util.TclModelUtil

import static org.mockito.Matchers.*
import static org.mockito.Mockito.*

class TclAssertCallBuilderTest extends AbstractParserTest {

	@InjectMocks TclAssertCallBuilder assertCallBuilder // class under test
	@Mock ModelUtil amlModelUtil // injected into class under test
	@Mock protected TclModelUtil tclModelUtil // injected into class under test
	@Mock TclExpressionBuilder expressionBuilder
	@Inject extension TclModelGenerator

	@Inject JvmTypeReferenceBuilder.Factory jvmTypeReferenceBuilderFactory
	
	@Before
	def void setupExpressionBuilder() {
		when(expressionBuilder.buildExpression(isA(StringConstant))).thenReturn('"test"')
		when(expressionBuilder.buildExpression(isA(VariableReference))).thenReturn('variable')
		when(expressionBuilder.buildExpression(isA(VariableReferenceMapAccess))).thenReturn('variable.get("key")')
	}

	@Test
	def void testEqualsGen() {
		// given
		val expression = flatReference("variable").compareOnEquality("test")

		// when
		val generatedCode = assertCallBuilder.build(expression)

		// then
		assertCodeLine('org.junit.Assert.assertEquals("", "test", variable);', generatedCode)
	}

	@Test
	def void testNotEqualsGen() {
		// given
		val expression = flatReference("variable").compareNotEqual("test")

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
		assertCodeLine('org.junit.Assert.assertFalse("", (variable != null) && variable.booleanValue());',
			generatedCode)
	}

	@Test
	def void testMatches() {
		// given
		val expression = flatReference("variable").compareMatching("test")

		// when
		val generatedCode = assertCallBuilder.build(expression)

		// then
		assertCodeLine('org.junit.Assert.assertTrue("", variable.toString().matches("test".toString()));', generatedCode)
	}

	@Test
	def void testDoesNotMatch() {
		// given
		val expression = flatReference("variable").compareNotMatching("test")

		// when
		val generatedCode = assertCallBuilder.build(expression)

		// then
		assertCodeLine('org.junit.Assert.assertFalse("", variable.toString().matches("test".toString()));', generatedCode)
	}

	@Test
	def void testWithMapDereference() {
		// given
		val expression = mappedReference("variable", "key").compareOnEquality("test")

		// when
		val generatedCode = assertCallBuilder.build(expression)

		// then
		assertCodeLine('org.junit.Assert.assertEquals("", "test", variable.get("key"));', generatedCode)
	}

	@Test
	def void testWithMapKeyAsString() {
		// given
		val expression = mappedReference("variable", "key with spaces").compareOnEquality("test")
		when(expressionBuilder.buildExpression(isA(VariableReferenceMapAccess))).thenReturn('variable.get("key with spaces")')

		// when
		val generatedCode = assertCallBuilder.build(expression)

		// then
		assertCodeLine('org.junit.Assert.assertEquals("", "test", variable.get("key with spaces"));', generatedCode)
	}

	/** make sure that questions to the type of the referenced variable within the assertion is set to clazz */
	private def void setVariableType(Class<?> clazz) {
		val jvmTypeReferenceBuilder = jvmTypeReferenceBuilderFactory.create(resourceSetProvider.get)
		val jvmType = jvmTypeReferenceBuilder.typeRef(clazz)

		when(amlModelUtil.getReturnType(any)).thenReturn(jvmType)
		when(amlModelUtil.isAssignableWithoutConversion(clazz, jvmType)).thenReturn(true)
	}

	// assert that the generated code holds 2 lines of which the second is identical to expectedCode
	private def void assertCodeLine(String expectedCode, String generatedCode) {
		val generatedCodeLines = generatedCode.split(System.lineSeparator)
		assertSize(generatedCodeLines, 2, "expecting two generated code lines, a comment and the assertion call")
		val assertMethod = generatedCodeLines.last
		assertMethod.assertEquals(expectedCode)
	}

}
