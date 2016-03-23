package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.junit.Test
import org.testeditor.tcl.dsl.tests.AbstractTest
import org.testeditor.tcl.impl.TclFactoryImpl

class TclAssertCallBuilderTest extends AbstractTest {
	@Inject extension TclAssertCallBuilder
	@Inject extension TclFactoryImpl

	@Test
	def void testEqualsGen() {
		// when
		val assertMethod = buildAssertCall(Boolean.FALSE, 'var', createComparatorEquals, '"test"')

		// then
		assertTrue(assertMethod.matches('org.junit.Assert.assertEquals\\(var, "test"\\);'))
	}

	@Test
	def void testEqualsGenWNullNegation() {
		// when
		val assertMethod = buildAssertCall(null, 'var', createComparatorEquals => [negated = false], '"test"')

		// then
		assertTrue(assertMethod.matches('org.junit.Assert.assertEquals\\(var, "test"\\);'))
	}

	@Test
	def void testNotEqualsGen() {
		// when
		val assertMethod = buildAssertCall(Boolean.TRUE, 'var', createComparatorEquals => [negated = false], '"test"')

		// then
		assertTrue(assertMethod.matches('org.junit.Assert.assertNotEquals\\(var, "test"\\);'))
	}

	@Test
	def void testInEqualityGen() {
		// when
		val assertMethod = buildAssertCall(Boolean.FALSE, 'var', createComparatorEquals => [negated = true], '"test"')

		// then
		assertTrue(assertMethod.matches('org.junit.Assert.assertNotEquals\\(var, "test"\\);'))
	}

	@Test
	def void testNotNullGen() {
		// when
		val assertMethod = buildAssertCall(null, 'var', null, null)

		// then
		assertTrue(assertMethod.matches('org.junit.Assert.assertNotNull\\(var\\);'))
	}

	@Test
	def void testNullGen() {
		// when
		val assertMethod = buildAssertCall(Boolean.TRUE, 'var', null, null)

		// then
		assertTrue(assertMethod.matches('org.junit.Assert.assertNull\\(var\\);'))
	}

	@Test
	def void testIncompleteImpl() {
		// when
		val assertMethod = buildAssertCall(Boolean.TRUE, 'var', createComparatorLessThen => [negated = true], '"test"')

		// then
		assertTrue(assertMethod.matches('// TODO .*'))
	}

	@Test
	def void testMatches() {
		// when
		val assertMethod = buildAssertCall(null, 'var', createComparatorMatches, '"test"')

		// then
		assertTrue(assertMethod.matches('org.junit.Assert.assertTrue\\(var.matches\\("test"\\)\\);'))
	}

	@Test
	def void testNegatedMatches() {
		// when
		val assertMethod = buildAssertCall(Boolean.TRUE, 'var', createComparatorMatches, '"test"')

		// then
		assertTrue(assertMethod.matches('org.junit.Assert.assertFalse\\(var.matches\\("test"\\)\\);'))
	}

	@Test
	def void testDoesNotMatch() {
		// when
		val assertMethod = buildAssertCall(null, 'var', createComparatorMatches => [negated = true], '"test"')

		// then
		assertTrue(assertMethod.matches('org.junit.Assert.assertFalse\\(var.matches\\("test"\\)\\);'))
	}

}
