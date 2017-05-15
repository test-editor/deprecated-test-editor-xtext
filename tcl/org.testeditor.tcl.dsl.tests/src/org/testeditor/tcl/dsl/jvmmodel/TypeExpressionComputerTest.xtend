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

import javax.inject.Inject
import org.eclipse.emf.ecore.resource.ResourceSet
import org.junit.Before
import org.junit.Test
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import org.testeditor.tsl.impl.TslFactoryImpl

import static org.mockito.Mockito.*

class TypeExpressionComputerTest extends AbstractTclTest{
	
	@InjectMocks TclExpressionTypeComputer expressionTypeComputer // class under test
	
	// need to inject this mock into expressionTypeComputer since any use of 
	// isAssignable within expressionTypeComputer makes use of a tclJvmTypeReferenceUtil 
	// that was initialized with the eResource of the eObject used, which will
	// result in "unusual" behaviour up to exceptions for dynamically created eObjects
	// without actual resource
	@Mock TclJvmTypeReferenceUtil innerTypeReferenceUtil // injected into expressionTypeComputer
	
	@Inject TclJvmTypeReferenceUtil typeReferenceUtil // used for construction of jvm type references for which the mock is not useful	
	@Inject TslFactoryImpl tslFactory
	
	@Before
	def void setupTypes() {
		typeReferenceUtil.initWith(null as ResourceSet) // allowed for test purposes
	}
	
	@Test
	def void testDetermineTypeOfStepContentVariableWithLong() {
		// given
		val content = tslFactory.createStepContentVariable => [ value = "123" ]
		val expectedType = typeReferenceUtil.longPrimitiveJvmTypeReference
		when(innerTypeReferenceUtil.isANumber(expectedType)).thenReturn(true)
		
		// when
		val result = expressionTypeComputer.determineType(content, expectedType)
		
		// then
		typeReferenceUtil.isLong(result).assertTrue		
	}

	@Test
	def void testDetermineTypeOfStepContentVariableWithInt() {
		// given
		val content = tslFactory.createStepContentVariable => [ value = "123" ]
		val expectedType = typeReferenceUtil.intPrimitiveJvmTypeReference
		when(innerTypeReferenceUtil.isANumber(expectedType)).thenReturn(true)
		
		// when
		val result = expressionTypeComputer.determineType(content, expectedType)
		
		// then
		typeReferenceUtil.isInt(result).assertTrue		
	}

	@Test
	def void testDetermineTypeOfStepContentVariableWithLongAsString() {
		// given
		val content = tslFactory.createStepContentVariable => [ value = "123" ]
		val expectedType = typeReferenceUtil.stringJvmTypeReference
		when(innerTypeReferenceUtil.isString(expectedType)).thenReturn(true)
		
		// when
		val result = expressionTypeComputer.determineType(content, expectedType)
		
		// then
		typeReferenceUtil.isString(result).assertTrue		
	}

	@Test
	def void testDetermineTypeOfStepContentVariableWithLongAsBoolean() {
		// given
		val content = tslFactory.createStepContentVariable => [ value = "123" ]
		val expectedType = typeReferenceUtil.booleanObjectJvmTypeReference
		when(innerTypeReferenceUtil.isBoolean(expectedType)).thenReturn(true)
		when(innerTypeReferenceUtil.longObjectJvmTypeReference).thenReturn(typeReferenceUtil.longObjectJvmTypeReference)
		
		// when
		val result = expressionTypeComputer.determineType(content, expectedType)
		
		// then
		typeReferenceUtil.isBoolean(result).assertFalse
		typeReferenceUtil.isLong(result).assertTrue // default derived from content
	}

	@Test
	def void testStepContentVariableIsANumber() {
		// given
		val content = tslFactory.createStepContentVariable => [value = "2"]
		val expectedType = typeReferenceUtil.bigDecimalJvmTypeReference
		when(innerTypeReferenceUtil.isANumber(expectedType)).thenReturn(true)

		// when
		val result = expressionTypeComputer.determineType(content, expectedType)

		// then
		typeReferenceUtil.isANumber(result).assertTrue
		typeReferenceUtil.isBigDecimal(result).assertTrue
	}
	
}