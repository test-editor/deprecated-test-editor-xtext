package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.junit.Test
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import org.testeditor.tcl.dsl.tests.TclModelGenerator
import org.junit.Before

class TclExpressionBuilderTest extends AbstractTclTest {

	@Inject TclExpressionBuilder expressionBuilder 
	@Inject IdenticalVariableResolver identicalVariableResolver
	@Inject extension TclModelGenerator
	
	@Before
	def void setUp() {
		expressionBuilder.variableResolver = identicalVariableResolver
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
