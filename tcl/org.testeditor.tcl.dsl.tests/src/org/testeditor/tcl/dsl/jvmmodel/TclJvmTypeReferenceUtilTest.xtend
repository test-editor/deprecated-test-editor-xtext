package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.xbase.typesystem.conformance.TypeConformanceComputationArgument
import org.junit.Before
import org.junit.Ignore
import org.junit.Test

class TclJvmTypeReferenceUtilTest extends AbstractTclGeneratorIntegrationTest{

	@Inject TclJvmTypeReferenceUtil classUnderTest
	@Inject TclJvmTypeReferenceUtil utilForTypeGenerationComparison
	
	val standardTestFlags = new TypeConformanceComputationArgument(false, false, false, false, false, true)
	
	@Before 
	def void initClassUnderTest() {
		classUnderTest.initWith(null as ResourceSet)
		utilForTypeGenerationComparison.initWith(null as ResourceSet)
	}
	
	@Test
	def void testBooleanToBooleanObjectAssignmentWithoutBoxing() {
		// given + when				
		val isAssignable = classUnderTest.isAssignableFrom(classUnderTest.booleanObjectJvmTypeReference,
			classUnderTest.buildFrom(Boolean), standardTestFlags)

		// then
		isAssignable.assertTrue
	}

	@Test
	def void testPrimitiveToBooleanObjectAssignmentWithoutBoxing() {
		// given + when		
		val isAssignable = classUnderTest.isAssignableFrom(classUnderTest.booleanObjectJvmTypeReference,
			classUnderTest.booleanPrimitiveJvmTypeReference, standardTestFlags)

		// then
		isAssignable.assertFalse
	}

	@Test
	def void testJsonElementToBooleanObjectAssignmentWithoutBoxing() {
		// given + when		
		val isAssignable = classUnderTest.isAssignableFrom(classUnderTest.booleanObjectJvmTypeReference,
			classUnderTest.jsonElementJvmTypeReference, standardTestFlags)

		// then
		isAssignable.assertFalse
	}

	@Ignore // this test should run within a (sane java) project and its resourceSet but fails locally with an exception 
	@Test
	def void testPrimitiveToBooleanObjectAssignmentWithBoxing() {
		// given + when		
		val isAssignable = classUnderTest.isAssignableFrom(classUnderTest.booleanObjectJvmTypeReference,
			classUnderTest.booleanPrimitiveJvmTypeReference)

		// then
		isAssignable.assertTrue
	}

	@Test
	def void testTypeEquality() {
		// given
		val jsonArray = classUnderTest.jsonArrayJvmTypeReference
		val myJsonArray = utilForTypeGenerationComparison.jsonArrayJvmTypeReference
		assertTrue(jsonArray !== myJsonArray) // be sure to use two different instances!
		
		// when
		val result = classUnderTest.isJsonArray(myJsonArray)
		
		// then
		result.assertTrue
	}
	
	@Test
	def void testTypeRecognition() {
		// given + when + then
		classUnderTest.isBigDecimal(classUnderTest.bigDecimalJvmTypeReference).assertTrue
		classUnderTest.isBigDecimal(classUnderTest.longObjectJvmTypeReference).assertFalse
		
		classUnderTest.isLong(classUnderTest.longObjectJvmTypeReference).assertTrue
		classUnderTest.isLong(classUnderTest.longPrimitiveJvmTypeReference).assertTrue
		classUnderTest.isLong(classUnderTest.bigDecimalJvmTypeReference).assertFalse
		
		classUnderTest.isInt(classUnderTest.intObjectJvmTypeReference).assertTrue
		classUnderTest.isInt(classUnderTest.intPrimitiveJvmTypeReference).assertTrue
		classUnderTest.isInt(classUnderTest.bigDecimalJvmTypeReference).assertFalse
		
		classUnderTest.isBoolean(classUnderTest.booleanObjectJvmTypeReference).assertTrue
		classUnderTest.isBoolean(classUnderTest.booleanPrimitiveJvmTypeReference).assertTrue
		classUnderTest.isBoolean(classUnderTest.stringJvmTypeReference).assertFalse
		
		classUnderTest.isString(classUnderTest.stringJvmTypeReference).assertTrue
		classUnderTest.isString(classUnderTest.booleanObjectJvmTypeReference).assertFalse
		
		classUnderTest.isJson(classUnderTest.jsonArrayJvmTypeReference).assertTrue
		classUnderTest.isJson(classUnderTest.stringJvmTypeReference).assertFalse
		
		classUnderTest.isJsonArray(classUnderTest.jsonArrayJvmTypeReference).assertTrue
		classUnderTest.isJsonArray(classUnderTest.stringJvmTypeReference).assertFalse
		classUnderTest.isJsonElement(classUnderTest.jsonElementJvmTypeReference).assertTrue
		classUnderTest.isJsonElement(classUnderTest.stringJvmTypeReference).assertFalse
		classUnderTest.isJsonObject(classUnderTest.jsonObjectJvmTypeReference).assertTrue
		classUnderTest.isJsonObject(classUnderTest.stringJvmTypeReference).assertFalse
		classUnderTest.isJsonPrimitive(classUnderTest.jsonPrimitiveJvmTypeReference).assertTrue
		classUnderTest.isJsonPrimitive(classUnderTest.stringJvmTypeReference).assertFalse
		
		classUnderTest.isANumber(classUnderTest.intObjectJvmTypeReference).assertTrue
		classUnderTest.isANumber(classUnderTest.longObjectJvmTypeReference).assertTrue
		classUnderTest.isANumber(classUnderTest.bigDecimalJvmTypeReference).assertTrue
		
		classUnderTest.isNumber(classUnderTest.numberJvmTypeReference).assertTrue
		classUnderTest.isNumber(classUnderTest.bigDecimalJvmTypeReference).assertFalse

		classUnderTest.isOrderable(classUnderTest.bigDecimalJvmTypeReference).assertTrue
		classUnderTest.isOrderable(classUnderTest.intPrimitiveJvmTypeReference).assertTrue
		classUnderTest.isOrderable(classUnderTest.longPrimitiveJvmTypeReference).assertTrue
		classUnderTest.isOrderable(classUnderTest.stringJvmTypeReference).assertFalse // strings are currently not orderable (<, <=, >, >=)
	}
	
}
