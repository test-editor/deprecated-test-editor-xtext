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
	
	@Before 
	def void initClassUnderTest() {
		classUnderTest.initWith(null as ResourceSet)
		utilForTypeGenerationComparison.initWith(null as ResourceSet)
	}
	
	@Test
	def void testBooleanToBooleanObjectAssignmentWithoutBoxing() {
		// given + when				
		val isAssignable = classUnderTest.isAssignableFrom(classUnderTest.booleanObjectJvmTypeReference,
			classUnderTest.buildFrom(Boolean),
			new TypeConformanceComputationArgument(false, false, false, false, false, true))

		// then
		isAssignable.assertTrue
	}

	@Test
	def void testPrimitiveToBooleanObjectAssignmentWithoutBoxing() {
		// given + when		
		val isAssignable = classUnderTest.isAssignableFrom(classUnderTest.booleanObjectJvmTypeReference,
			classUnderTest.booleanPrimitiveJvmTypeReference,
			new TypeConformanceComputationArgument(false, false, false, false, false, true))

		// then
		isAssignable.assertFalse
	}

	@Test
	def void testJsonElementToBooleanObjectAssignmentWithoutBoxing() {
		// given + when		
		val isAssignable = classUnderTest.isAssignableFrom(classUnderTest.booleanObjectJvmTypeReference,
			classUnderTest.jsonElementJvmTypeReference,
			new TypeConformanceComputationArgument(false, false, false, false, false, true))

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
	
}
