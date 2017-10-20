/*******************************************************************************
 * Copyright (c) 2012 - 2017 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 *******************************************************************************/
package org.testeditor.tcl.dsl.jvmmodel

import java.util.Optional
import javax.inject.Inject
import org.eclipse.xtext.common.types.JvmTypeReference
import org.junit.Before
import org.junit.Test
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.aml.Variable
import org.testeditor.tcl.Expression
import org.testeditor.tcl.KeyPathElement
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import org.testeditor.tcl.dsl.tests.TclModelGenerator

import static org.mockito.Matchers.*
import static org.mockito.Mockito.*

class TclExpressionBuilderTest extends AbstractTclTest {

	@InjectMocks TclExpressionBuilder expressionBuilder // class under test
	
	@Mock TclJsonUtil tclJsonUtil // injected into expressionBuilder
	@Mock TclExpressionTypeComputer typeComputer // injected into expression Builder
	@Mock TclCoercionComputer tclCoercionComputer // injected into expression Builder
	
	@Inject extension TclModelGenerator
	@Mock JvmTypeReference stringTypeReference
	
	val someKey = "Some Key"
	
	@Before
	def void prepareMocks() {
		when(stringTypeReference.qualifiedName).thenReturn(String.name)
		when(typeComputer.determineType(any(Expression),any(Optional))).thenReturn(stringTypeReference)
		when(typeComputer.determineType(any(Variable),any(Optional))).thenReturn(stringTypeReference)
		when(typeComputer.coercedTypeOfComparison(any, any)).thenReturn(stringTypeReference)
		when(tclJsonUtil.jsonPathReadAccessToString(any(KeyPathElement))).thenReturn('''.getJsonObject().get("«someKey»")''')
		when(tclCoercionComputer.generateCoercion(stringTypeReference, stringTypeReference, '"test"')).
			thenReturn('"test"')
		when(tclCoercionComputer.generateCoercion(stringTypeReference, stringTypeReference, 'variable')).thenReturn(
			'variable')
	}
	
	@Test
	def void testMatches() {
		// given
		val matchingComparison = compareMatching(variableReference => [variable = assignmentVariable("variable")],
			"test")

		// when
		val result = expressionBuilder.buildReadExpression(matchingComparison)

		// then
		result.assertEquals('variable.toString().matches("test".toString())')
	}

	@Test
	def void testEquals() {
		// given
		val equal = compareOnEquality(variableReference => [variable = assignmentVariable("variable")], "test")

		// when
		val result = expressionBuilder.buildReadExpression(equal)

		// then
		result.assertEquals('variable == "test"')
	}

	@Test
	def void testNotEqual() {
		// given
		val notEqual = compareNotEqual(variableReference => [variable = assignmentVariable("variable")], "test")

		// when
		val result = expressionBuilder.buildReadExpression(notEqual)

		// then
		result.assertEquals('variable != "test"')
	}

	@Test
	def void testJsonReference() {
		// given
		val jsonMapAccess = variableReferencePathAccess("variable", someKey)

		// when
		val result = expressionBuilder.buildReadExpression(jsonMapAccess)

		// then
		result.assertEquals('''variable.getJsonObject().get("«someKey»")'''.toString)
	}

	@Test
	def void testEnvironmentReference() {
		// given
		val envVar = variableReference => [variable = environmentVariablesPublic("variable").head]

		// when
		val result = expressionBuilder.buildReadExpression(envVar)

		// then
		result.assertEquals('env_variable')
	}

}
