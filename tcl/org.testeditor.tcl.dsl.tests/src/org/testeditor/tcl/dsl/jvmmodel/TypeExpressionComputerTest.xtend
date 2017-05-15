package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.eclipse.emf.ecore.resource.ResourceSet
import org.junit.Before
import org.junit.Test
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import org.testeditor.tsl.impl.TslFactoryImpl

class TypeExpressionComputerTest extends AbstractTclTest{
	
	@Inject TclExpressionTypeComputer expressionTypeComputer // class under test
	
	@Inject TclJvmTypeReferenceUtil typeReferenceUtil
	@Inject TslFactoryImpl tslFactory
	
	@Before
	def void setupTypes() {
		typeReferenceUtil.initWith(null as ResourceSet) // allowed for test purposes
	}
	
	@Test
	def void testDetermineTypeOfStepContentVariableWithLong() {
		// given
		val content = tslFactory.createStepContentVariable => [ value = "123" ]
		
		// when
		val result = expressionTypeComputer.determineType(content, typeReferenceUtil.longPrimitiveJvmTypeReference)
		
		// then
		typeReferenceUtil.isLong(result).assertTrue		
	}

	@Test
	def void testDetermineTypeOfStepContentVariableWithInt() {
		// given
		val content = tslFactory.createStepContentVariable => [ value = "123" ]
		
		// when
		val result = expressionTypeComputer.determineType(content, typeReferenceUtil.intPrimitiveJvmTypeReference)
		
		// then
		typeReferenceUtil.isInt(result).assertTrue		
	}

	@Test
	def void testDetermineTypeOfStepContentVariableWithLongAsString() {
		// given
		val content = tslFactory.createStepContentVariable => [ value = "123" ]
		
		// when
		val result = expressionTypeComputer.determineType(content, typeReferenceUtil.stringJvmTypeReference)
		
		// then
		typeReferenceUtil.isString(result).assertTrue		
	}

	@Test
	def void testDetermineTypeOfStepContentVariableWithLongAsBoolean() {
		// given
		val content = tslFactory.createStepContentVariable => [ value = "123" ]
		
		// when
		val result = expressionTypeComputer.determineType(content, typeReferenceUtil.booleanObjectJvmTypeReference)
		
		// then
		typeReferenceUtil.isBoolean(result).assertFalse		
		typeReferenceUtil.isLong(result).assertTrue // default derived from content
	}

}