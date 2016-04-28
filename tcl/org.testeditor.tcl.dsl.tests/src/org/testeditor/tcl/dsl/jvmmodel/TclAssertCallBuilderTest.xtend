package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.junit.Test
import org.testeditor.tcl.AssertionTestStep
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest

class TclAssertCallBuilderTest extends AbstractParserTest {

	@Inject TclAssertCallBuilder assertCallBuilder

	@Test
	def void testEqualsGen() {
		// given
		val assertionTestStep = parseAssertionTestStep('''- assert variable == "test"''')

		// when
		val assertMethod = assertCallBuilder.build(assertionTestStep.expression)

		// then
		assertMethod.assertEquals('org.junit.Assert.assertEquals(variable, "test");')
	}

	@Test
	def void testNotEqualsGen() {
		// given
		val assertionTestStep = parseAssertionTestStep('''- assert variable != "test"''')

		// when
		val assertMethod = assertCallBuilder.build(assertionTestStep.expression)

		// then
		assertMethod.assertEquals('org.junit.Assert.assertNotEquals(variable, "test");')
	}

	@Test
	def void testInEqualityGen() {
		// given
		val assertionTestStep = parseAssertionTestStep('''- assert variable is not "test"''')

		// when
		val assertMethod = assertCallBuilder.build(assertionTestStep.expression)

		// then
		assertMethod.assertEquals('org.junit.Assert.assertNotEquals(variable, "test");')
	}

	@Test
	def void testNotNullGen() {
		// given
		val assertionTestStep = parseAssertionTestStep('''- assert variable''')

		// when
		val assertMethod = assertCallBuilder.build(assertionTestStep.expression)

		// then
		assertMethod.assertEquals('org.junit.Assert.assertNotNull(variable);')
	}

	@Test
	def void testNullGen() {
		// given
		val assertionTestStep = parseAssertionTestStep('''- assert !variable''')

		// when
		val assertMethod = assertCallBuilder.build(assertionTestStep.expression)

		// then
		assertMethod.assertEquals('org.junit.Assert.assertNull(variable);')
	}

	@Test
	def void testIncompleteImpl() {
		// given
		val assertionTestStep = parseAssertionTestStep('''- assert variable > "ohoh"''')

		// when
		val assertMethod = assertCallBuilder.build(assertionTestStep.expression)

		// then
		assertTrue(assertMethod.matches('// TODO .*'))
	}

	@Test
	def void testMatches() {
		// given
		val assertionTestStep = parseAssertionTestStep('''- assert variable matches "ohoh"''')

		// when
		val assertMethod = assertCallBuilder.build(assertionTestStep.expression)

		// then
		assertMethod.assertEquals('org.junit.Assert.assertTrue(variable.matches("ohoh"));')
	}

	@Test
	def void testDoesNotMatch() {
		// given
		val assertionTestStep = parseAssertionTestStep('''- assert variable does not match "ohoh"''')

		// when
		val assertMethod = assertCallBuilder.build(assertionTestStep.expression)

		// then
		assertMethod.assertEquals('org.junit.Assert.assertFalse(variable.matches("ohoh"));')
	}

	@Test
	def void testWithMapDereference() {
		// given
		val assertionTestStep = parseAssertionTestStep('''- assert variable.key == "test"''')

		// when
		val assertMethod = assertCallBuilder.build(assertionTestStep.expression)

		// then
		assertMethod.assertEquals('org.junit.Assert.assertEquals(variable.get("key"), "test");')
	}

	@Test
	def void testWithMapKeyAsString() {
		// given
		val assertionTestStep = parseAssertionTestStep('''- assert variable."key with spaces" == "test"''')

		// when
		val assertMethod = assertCallBuilder.build(assertionTestStep.expression)

		// then
		assertMethod.assertEquals('org.junit.Assert.assertEquals(variable.get("key with spaces"), "test");')
	}

	private def AssertionTestStep parseAssertionTestStep(CharSequence seq) {
		return seq.parse(grammarAccess.assertionTestStepRule, AssertionTestStep)
	}

}
