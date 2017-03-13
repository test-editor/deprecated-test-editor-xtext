package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.eclipse.xtext.common.types.JvmTypeReference
import org.junit.Before
import org.junit.Test
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.aml.Variable
import org.testeditor.tcl.Expression
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import org.testeditor.tcl.dsl.tests.TclModelGenerator

import static org.mockito.Matchers.*
import static org.mockito.Mockito.*

class TclExpressionBuilderTest extends AbstractTclTest {

	@InjectMocks TclExpressionBuilder expressionBuilder
	@Mock TclExpressionTypeComputer typeComputer
	@Inject extension TclModelGenerator
	@Mock JvmTypeReference stringTypeReference
	
	
	@Before
	def void prepareMocks() {
		when(stringTypeReference.qualifiedName).thenReturn(String.name)
		when(typeComputer.determineType(any(Expression))).thenReturn(stringTypeReference)
		when(typeComputer.determineType(any(Variable))).thenReturn(stringTypeReference)
		when(typeComputer.coercedTypeOfComparison(any)).thenReturn(stringTypeReference)
	}
	
	@Test
	def void testMatches() {
		// given
		val matchingComparison = compareMatching(variableReference => [variable = assignmentVariable("variable")],
			"test")

		// when
		val result = expressionBuilder.buildExpression(matchingComparison)

		// then
		result.assertEquals('variable.toString().matches("test".toString())')
	}

	@Test
	def void testEquals() {
		// given
		val equal = compareOnEquality(variableReference => [variable = assignmentVariable("variable")], "test")

		// when
		val result = expressionBuilder.buildExpression(equal)

		// then
		result.assertEquals('variable == "test"')
	}

	@Test
	def void testNotEqual() {
		// given
		val notEqual = compareNotEqual(variableReference => [variable = assignmentVariable("variable")], "test")

		// when
		val result = expressionBuilder.buildExpression(notEqual)

		// then
		result.assertEquals('variable != "test"')
	}

	@Test
	def void testMapReference() {
		// given
		val mapAccess = mappedReference("variable", "key with spaces")

		// when
		val result = expressionBuilder.buildExpression(mapAccess)

		// then
		result.assertEquals('variable.get("key with spaces")')
	}

	@Test
	def void testEnvironmentReference() {
		// given
		val envVar = variableReference => [variable = environmentVariables("variable").head]

		// when
		val result = expressionBuilder.buildExpression(envVar)

		// then
		result.assertEquals('env_variable')
	}

}
