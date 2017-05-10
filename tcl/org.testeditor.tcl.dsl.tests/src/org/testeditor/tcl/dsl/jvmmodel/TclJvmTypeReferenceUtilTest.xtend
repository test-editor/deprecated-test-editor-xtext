package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.eclipse.xtext.xbase.typesystem.conformance.TypeConformanceComputationArgument
import org.junit.Test
import org.eclipse.emf.ecore.resource.ResourceSet
import org.junit.Before
import org.junit.Ignore

class TclJvmTypeReferenceUtilTest extends AbstractTclGeneratorIntegrationTest{

	@Inject ResourceSet resourceSet
	@Inject TclJvmTypeReferenceUtil classUnderTest
	
	@Before 
	def void initClassUnderTest() {
		classUnderTest.initWith(resourceSet)
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

	@Ignore // currently works only if initialized with null resource set
	@Test
	def void testPrimitiveToBooleanObjectAssignmentWithBoxing() {
		// given + when		
		val isAssignable = classUnderTest.isAssignableFrom(classUnderTest.booleanObjectJvmTypeReference,
			classUnderTest.booleanPrimitiveJvmTypeReference)

		// then
		isAssignable.assertTrue
	}
	
}
