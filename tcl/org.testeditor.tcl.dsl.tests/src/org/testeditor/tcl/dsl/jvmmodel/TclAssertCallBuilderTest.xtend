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

import com.google.inject.Provider
import javax.inject.Inject
import org.eclipse.xtext.resource.XtextResourceSet
import org.eclipse.xtext.xbase.jvmmodel.JvmTypeReferenceBuilder
import org.junit.Before
import org.junit.Test
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.tcl.JsonString
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.VariableReferencePathAccess
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import org.testeditor.tcl.dsl.tests.TclModelGenerator
import org.testeditor.tcl.util.TclModelUtil

import static org.mockito.Matchers.*
import static org.mockito.Mockito.*

class TclAssertCallBuilderTest extends AbstractTclTest {

	@InjectMocks TclAssertCallBuilder assertCallBuilder // class under test
	
	@Mock protected TclModelUtil tclModelUtil // injected into class under test
	@Mock TclExpressionBuilder expressionBuilder // injected into class under test
	@Mock SimpleTypeComputer simpleTypeComputer // injected into class under test
	@Mock VariableCollector variableCollector // injected into class under test	
	@Mock TclExpressionTypeComputer tclExpressionTypeComputer // injected into class under test
	@Mock TclJvmTypeReferenceUtil tclJvmTypeReferenceUtil // injected into class under test
	
	@Inject extension TclModelGenerator
	@Inject protected Provider<XtextResourceSet> resourceSetProvider
		
	@Inject JvmTypeReferenceBuilder.Factory jvmTypeReferenceBuilderFactory
	
	static val VARIABLE_NAME = 'variable'
	static val STRING_FOR_COMPARISON = 'test'
	
	@Before
	def void setupExpressionBuilder() {
		when(expressionBuilder.buildReadExpression(isA(JsonString),any)).thenReturn('''"«STRING_FOR_COMPARISON»"''')
		when(expressionBuilder.buildReadExpression(isA(VariableReference),any)).thenReturn(VARIABLE_NAME)
		when(expressionBuilder.buildReadExpression(isA(JsonString))).thenReturn('''"«STRING_FOR_COMPARISON»"''')
		when(expressionBuilder.buildReadExpression(isA(VariableReference))).thenReturn(VARIABLE_NAME)
		when(expressionBuilder.buildReadExpression(isA(VariableReferencePathAccess))).thenReturn(VARIABLE_NAME+'.get("key")')
	}

	@Test
	def void testEqualsGen() {
		// given
		val expression = flatReference(VARIABLE_NAME).compareOnEquality(STRING_FOR_COMPARISON)

		// when
		val generatedCode = assertCallBuilder.build(expression, "prefix")

		// then
		assertCodeLine('''org.junit.Assert.assertEquals("prefix: ", "«STRING_FOR_COMPARISON»", «VARIABLE_NAME»);''', generatedCode)
	}

	@Test
	def void testNotEqualsGen() {
		// given
		val expression = flatReference(VARIABLE_NAME).compareNotEqual(STRING_FOR_COMPARISON)

		// when
		val generatedCode = assertCallBuilder.build(expression, "prefix")

		// then
		assertCodeLine('''org.junit.Assert.assertNotEquals("prefix: ", "«STRING_FOR_COMPARISON»", «VARIABLE_NAME»);''', generatedCode)
	}

	@Test
	def void testNotNullGen() {
		// given
		variableType = String
		val expression = nullOrBoolCheck(VARIABLE_NAME)

		// when
		val generatedCode = assertCallBuilder.build(expression, "prefix")

		// then
		assertCodeLine('''org.junit.Assert.assertNotNull("prefix: ", «VARIABLE_NAME»);''', generatedCode)
	}

	@Test
	def void testNullGen() {
		// given
		variableType = String
		val expression = nullOrBoolCheck(VARIABLE_NAME) => [negated = true]

		// when
		val generatedCode = assertCallBuilder.build(expression, "prefix")

		// then
		assertCodeLine('''org.junit.Assert.assertNull("prefix: ", «VARIABLE_NAME»);''', generatedCode)
	}

	@Test
	def void testBooleanValue() {
		// given
		variableType = boolean
		val expression = nullOrBoolCheck(VARIABLE_NAME)

		// when
		val generatedCode = assertCallBuilder.build(expression, "prefix")

		// then
		assertCodeLine('''org.junit.Assert.assertTrue("prefix: ", «VARIABLE_NAME»);''', generatedCode)
	}

	@Test
	def void testNotBooleanValue() {
		// given
		variableType = boolean
		val expression = nullOrBoolCheck(VARIABLE_NAME) => [negated = true]

		// when
		val generatedCode = assertCallBuilder.build(expression, "prefix")

		// then
		assertCodeLine('''org.junit.Assert.assertFalse("prefix: ", «VARIABLE_NAME»);''', generatedCode)
	}

	@Test
	def void testBooleanObject() {
		// given
		variableType = Boolean
		val expression = nullOrBoolCheck(VARIABLE_NAME)

		// when
		val generatedCode = assertCallBuilder.build(expression, "prefix")

		// then
		assertCodeLine('''org.junit.Assert.assertTrue("prefix: ", («VARIABLE_NAME» != null) && «VARIABLE_NAME».booleanValue());''', generatedCode)
	}

	@Test
	def void testNotBooleanObject() {
		// given
		variableType = Boolean
		val expression = nullOrBoolCheck(VARIABLE_NAME) => [negated = true]

		// when
		val generatedCode = assertCallBuilder.build(expression, "prefix")

		// then
		assertCodeLine('''org.junit.Assert.assertFalse("prefix: ", («VARIABLE_NAME» != null) && «VARIABLE_NAME».booleanValue());''',
			generatedCode)
	}

	@Test
	def void testMatches() {
		// given
		val expression = flatReference(VARIABLE_NAME).compareMatching(STRING_FOR_COMPARISON)

		// when
		val generatedCode = assertCallBuilder.build(expression, "prefix")

		// then
		assertCodeLine('''org.junit.Assert.assertTrue("prefix: ", «VARIABLE_NAME».toString().matches("«STRING_FOR_COMPARISON»".toString()));''',
			generatedCode)
	}

	@Test
	def void testDoesNotMatch() {
		// given
		val expression = flatReference(VARIABLE_NAME).compareNotMatching(STRING_FOR_COMPARISON)

		// when
		val generatedCode = assertCallBuilder.build(expression, "prefix")

		// then
		assertCodeLine('''org.junit.Assert.assertFalse("prefix: ", «VARIABLE_NAME».toString().matches("«STRING_FOR_COMPARISON»".toString()));''',
			generatedCode)
	}

	@Test
	def void testWithJsonObjectDereference() {
		// given
		val expression = variableReferencePathAccess(VARIABLE_NAME, "key").compareOnEquality(STRING_FOR_COMPARISON)
		when(expressionBuilder.buildReadExpression(isA(VariableReferencePathAccess),any)).thenReturn('''«VARIABLE_NAME».get("key")''')

		// when
		val generatedCode = assertCallBuilder.build(expression, "prefix")

		// then
		assertCodeLine('''org.junit.Assert.assertEquals("prefix: ", "«STRING_FOR_COMPARISON»", «VARIABLE_NAME».get("key"));''', generatedCode)
	}

	@Test
	def void testWithJsonObjectKeyAsString() {
		// given
		val expression = variableReferencePathAccess(VARIABLE_NAME, "key with spaces").compareOnEquality(STRING_FOR_COMPARISON)
		when(expressionBuilder.buildReadExpression(isA(VariableReferencePathAccess),any)).thenReturn('''«VARIABLE_NAME».get("key with spaces")''')

		// when
		val generatedCode = assertCallBuilder.build(expression, "prefix")

		// then
		assertCodeLine('''org.junit.Assert.assertEquals("prefix: ", "«STRING_FOR_COMPARISON»", «VARIABLE_NAME».get("key with spaces"));''', generatedCode)
	}

	/** make sure that questions to the type of the referenced variable within the assertion is set to clazz */
	private def void setVariableType(Class<?> clazz) {
		val jvmTypeReferenceBuilder = jvmTypeReferenceBuilderFactory.create(resourceSetProvider.get)
		val jvmType = jvmTypeReferenceBuilder.typeRef(clazz)

		when(variableCollector.collectDeclaredVariablesTypeMap(any)).thenReturn(#{VARIABLE_NAME->jvmType})
		when(tclJvmTypeReferenceUtil.isAssignableFrom(any, any, any)).thenReturn(clazz.name == Boolean.name)
	}

	// assert that the generated code holds 2 lines of which the second is identical to expectedCode
	private def void assertCodeLine(CharSequence expectedCode, String generatedCode) {
		val generatedCodeLines = generatedCode.split(System.lineSeparator)
		assertSize(generatedCodeLines, 1, "expecting one generated code line, just the assertion call")
		val assertMethod = generatedCodeLines.last
		assertMethod.assertEquals(expectedCode.toString)
	}

}
