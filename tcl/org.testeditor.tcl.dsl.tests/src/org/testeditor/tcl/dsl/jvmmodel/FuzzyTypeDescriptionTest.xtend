package org.testeditor.tcl.dsl.jvmmodel

import com.google.inject.Injector
import java.util.ArrayList
import java.util.List
import javax.inject.Inject
import org.junit.Test
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import org.junit.Ignore
import org.junit.Before
import org.eclipse.emf.ecore.resource.ResourceSet

@Ignore
class FuzzyTypeDescriptionTest extends AbstractTclTest {

	// @Inject TclExpressionTypeComputer expressionTypeComputer
	@Inject Injector injector
	@Inject TclJvmTypeReferenceUtil tclJvmTypeReferenceUtil
	
	
	@Before
	def void initRefUtil() {
		tclJvmTypeReferenceUtil.initWith(null as ResourceSet)
	}
	
	
	@Test
	def void testCreation() {
		// given
		val typeDescription = injector.getInstance(FuzzyTypeDescription)

		// when
		val allTypes = typeDescription.allTypes

		// then
		allTypes.assertEmpty
	}

	@Test
	def void testBestMatch() {
		// given
		val fuzzyDescriptionA = injector.getInstance(FuzzyTypeDescription) => [
			addType(tclJvmTypeReferenceUtil.stringJvmTypeReference, 100)
			addType(tclJvmTypeReferenceUtil.longObjectJvmTypeReference, 50)
		]
		val fuzzyDescriptionB = injector.getInstance(FuzzyTypeDescription) => [
			addType(tclJvmTypeReferenceUtil.longObjectJvmTypeReference, 60)
			addType(tclJvmTypeReferenceUtil.booleanObjectJvmTypeReference, 80)
		]

		// when
		val bestMatches = fuzzyDescriptionA.findBestMatches(fuzzyDescriptionB)

		// then
		bestMatches.assertSingleElement.assertEquals(tclJvmTypeReferenceUtil.longObjectJvmTypeReference)
	}

	@Test
	def void testBestMatchWithMultiMatchingSubclass() {
		// given		
		val objectTypeDescription = tclJvmTypeReferenceUtil.buildFrom(Object)
		
		val fuzzyDescriptionA = injector.getInstance(FuzzyTypeDescription) => [
			addType(tclJvmTypeReferenceUtil.stringJvmTypeReference, 50)
			addType(objectTypeDescription, 50)
		]
		val fuzzyDescriptionB = injector.getInstance(FuzzyTypeDescription) => [
			addType(tclJvmTypeReferenceUtil.stringJvmTypeReference, 20)
			addType(tclJvmTypeReferenceUtil.booleanPrimitiveJvmTypeReference, 80)
		]

		// when
		val bestMatches = fuzzyDescriptionA.findBestMatches(fuzzyDescriptionB)

		// then
		bestMatches.assertSingleElement.assertEquals(tclJvmTypeReferenceUtil.booleanPrimitiveJvmTypeReference)
	}

	@Test
	def void testBestMatchWithNonAssignableGenerics() {
		// given
		val listDescription = tclJvmTypeReferenceUtil.buildFrom(List,tclJvmTypeReferenceUtil.stringJvmTypeReference)
		val arrayListDescription = tclJvmTypeReferenceUtil.buildFrom(ArrayList,tclJvmTypeReferenceUtil.booleanObjectJvmTypeReference)

		val fuzzyDescriptionA = injector.getInstance(FuzzyTypeDescription) => [
			addType(listDescription, 50)
		]
		val fuzzyDescriptionB = injector.getInstance(FuzzyTypeDescription) => [
			addType(listDescription, 10)
			addType(arrayListDescription, 20)
		]

		// when
		val bestMatches = fuzzyDescriptionA.findBestMatches(fuzzyDescriptionB)

		// then
		bestMatches.assertSingleElement.assertEquals(listDescription)
	}

	@Test
	def void testBestMatchWithAssignableGenerics() {
		// given
		val listDescription = tclJvmTypeReferenceUtil.buildFrom(List,tclJvmTypeReferenceUtil.stringJvmTypeReference)
		val arrayListDescription = tclJvmTypeReferenceUtil.buildFrom(ArrayList,tclJvmTypeReferenceUtil.stringJvmTypeReference)

		val fuzzyDescriptionA = injector.getInstance(FuzzyTypeDescription) => [
			addType(listDescription, 50)
		]
		val fuzzyDescriptionB = injector.getInstance(FuzzyTypeDescription) => [
			addType(listDescription, 10)
			addType(arrayListDescription, 20)
		]

		// when
		val bestMatches = fuzzyDescriptionA.findBestMatches(fuzzyDescriptionB)

		// then
		bestMatches.assertSingleElement.assertEquals(arrayListDescription)
	}

	@Test
	def void testBestMatchWithSingleMatchingSubclass() {
		// given
		val charSequenceTypeDescription = tclJvmTypeReferenceUtil.buildFrom(CharSequence)

		val fuzzyDescriptionA = injector.getInstance(FuzzyTypeDescription) => [
			addType(tclJvmTypeReferenceUtil.booleanObjectJvmTypeReference, 20)
			addType(charSequenceTypeDescription, 50)
		]
		val fuzzyDescriptionB = injector.getInstance(FuzzyTypeDescription) => [
			addType(tclJvmTypeReferenceUtil.stringJvmTypeReference, 70)
			addType(tclJvmTypeReferenceUtil.booleanObjectJvmTypeReference, 80)
		]

		// when
		val bestMatches = fuzzyDescriptionA.findBestMatches(fuzzyDescriptionB)

		// then
		bestMatches.assertSingleElement.assertEquals(tclJvmTypeReferenceUtil.stringJvmTypeReference)
	}

	@Test
	def void testMatch() {
		// given
		val otherStringTypeDescription = tclJvmTypeReferenceUtil.buildFrom(String)

		val fuzzyDescription = injector.getInstance(FuzzyTypeDescription) => [addType(tclJvmTypeReferenceUtil.stringJvmTypeReference)]

		// when
		val doesMatch = fuzzyDescription.matches(otherStringTypeDescription)

		// then
		doesMatch.assertTrue
	}

	@Test
	def void testMismatch() {
		// given
		val otherTypeDescription = tclJvmTypeReferenceUtil.buildFrom(Boolean)

		val fuzzyDescription = injector.getInstance(FuzzyTypeDescription) => [addType(tclJvmTypeReferenceUtil.stringJvmTypeReference)]

		// when
		val doesMatch = fuzzyDescription.matches(otherTypeDescription)

		// then
		doesMatch.assertFalse
	}
}
