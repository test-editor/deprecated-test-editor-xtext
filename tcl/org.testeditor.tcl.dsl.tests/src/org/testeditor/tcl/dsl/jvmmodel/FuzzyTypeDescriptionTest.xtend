package org.testeditor.tcl.dsl.jvmmodel

import com.google.inject.Injector
import java.util.ArrayList
import javax.inject.Inject
import org.eclipse.xtext.common.types.JvmTypeReference
import org.junit.Test
import org.testeditor.tcl.dsl.tests.AbstractTclTest

class FuzzyTypeDescriptionTest extends AbstractTclTest {

	@Inject TclExpressionTypeComputer expressionTypeComputer
	@Inject Injector injector

	private def JvmTypeReference buildFrom(Class<?> clazz, JvmTypeReference ... typeArgs) {
		expressionTypeComputer.buildFrom(clazz, typeArgs)
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
		val stringTypeDescription = String.buildFrom
		val longTypeDescription = Long.buildFrom
		val booleanTypeDescription = Boolean.buildFrom

		val fuzzyDescriptionA = injector.getInstance(FuzzyTypeDescription) => [
			addType(stringTypeDescription, 100)
			addType(longTypeDescription, 50)
		]
		val fuzzyDescriptionB = injector.getInstance(FuzzyTypeDescription) => [
			addType(longTypeDescription, 60)
			addType(booleanTypeDescription, 80)
		]

		// when
		val bestMatches = fuzzyDescriptionA.findBestMatches(fuzzyDescriptionB)

		// then
		bestMatches.assertSingleElement.assertEquals(longTypeDescription)
	}

	@Test
	def void testBestMatchWithMultiMatchingSubclass() {
		// given
		val stringTypeDescription = String.buildFrom
		val objectTypeDescription = Object.buildFrom
		val booleanTypeDescription = Boolean.buildFrom

		val fuzzyDescriptionA = injector.getInstance(FuzzyTypeDescription) => [
			addType(stringTypeDescription, 50)
			addType(objectTypeDescription, 50)
		]
		val fuzzyDescriptionB = injector.getInstance(FuzzyTypeDescription) => [
			addType(stringTypeDescription, 20)
			addType(booleanTypeDescription, 80)
		]

		// when
		val bestMatches = fuzzyDescriptionA.findBestMatches(fuzzyDescriptionB)

		// then
		bestMatches.assertSingleElement.assertEquals(booleanTypeDescription)
	}

	// dummy class to allow inheritance assignment tests for classes with generics
	static class FinalArrayList<T> extends ArrayList<T> {
	}

	@Test
	def void testBestMatchWithNonAssignableGenerics() {
		// given
		val stringTypeDescription = String.buildFrom // used as type parameter for arrayList
		val booleanTypeDescription = Boolean.buildFrom // used as type parameter for finalArrayList
		val ArrayList<String> arrayList = newArrayList
		val arrayListDescription = arrayList.class.buildFrom(stringTypeDescription)
		val FinalArrayList<Boolean> finalArrayList = new FinalArrayList
		val finalArrayListDescription = finalArrayList.class.buildFrom(booleanTypeDescription)

		val fuzzyDescriptionA = injector.getInstance(FuzzyTypeDescription) => [
			addType(arrayListDescription, 50)
		]
		val fuzzyDescriptionB = injector.getInstance(FuzzyTypeDescription) => [
			addType(arrayListDescription, 10)
			addType(finalArrayListDescription, 20)
		]

		// when
		val bestMatches = fuzzyDescriptionA.findBestMatches(fuzzyDescriptionB)

		// then
		bestMatches.assertSingleElement.assertEquals(arrayListDescription)
	}

	@Test
	def void testBestMatchWithAssignableGenerics() {
		// given
		val stringTypeDescription = String.buildFrom // used as type parameter for arrayList
		val ArrayList<String> arrayList = newArrayList
		val arrayListDescription = arrayList.class.buildFrom(stringTypeDescription)
		val FinalArrayList<String> finalArrayList = new FinalArrayList
		val finalArrayListDescription = finalArrayList.class.buildFrom(stringTypeDescription)

		val fuzzyDescriptionA = injector.getInstance(FuzzyTypeDescription) => [
			addType(arrayListDescription, 50)
		]
		val fuzzyDescriptionB = injector.getInstance(FuzzyTypeDescription) => [
			addType(arrayListDescription, 10)
			addType(finalArrayListDescription, 20)
		]

		// when
		val bestMatches = fuzzyDescriptionA.findBestMatches(fuzzyDescriptionB)

		// then
		bestMatches.assertSingleElement.assertEquals(finalArrayListDescription)
	}

	@Test
	def void testBestMatchWithSingleMatchingSubclass() {
		// given
		val stringTypeDescription = String.buildFrom
		val charSequenceTypeDescription = CharSequence.buildFrom
		val booleanTypeDescription = Boolean.buildFrom

		val fuzzyDescriptionA = injector.getInstance(FuzzyTypeDescription) => [
			addType(booleanTypeDescription, 20)
			addType(charSequenceTypeDescription, 50)
		]
		val fuzzyDescriptionB = injector.getInstance(FuzzyTypeDescription) => [
			addType(stringTypeDescription, 70)
			addType(booleanTypeDescription, 80)
		]

		// when
		val bestMatches = fuzzyDescriptionA.findBestMatches(fuzzyDescriptionB)

		// then
		bestMatches.assertSingleElement.assertEquals(stringTypeDescription)
	}

	@Test
	def void testMatch() {
		// given
		val stringTypeDescription = String.buildFrom
		val otherStringTypeDescription = String.buildFrom

		val fuzzyDescription = injector.getInstance(FuzzyTypeDescription) => [addType(stringTypeDescription)]

		// when
		val doesMatch = fuzzyDescription.matches(otherStringTypeDescription)

		// then
		doesMatch.assertTrue
	}

	@Test
	def void testMismatch() {
		// given
		val stringTypeDescription = String.buildFrom
		val otherTypeDescription = Boolean.buildFrom

		val fuzzyDescription = injector.getInstance(FuzzyTypeDescription) => [addType(stringTypeDescription)]

		// when
		val doesMatch = fuzzyDescription.matches(otherTypeDescription)

		// then
		doesMatch.assertFalse
	}
}
