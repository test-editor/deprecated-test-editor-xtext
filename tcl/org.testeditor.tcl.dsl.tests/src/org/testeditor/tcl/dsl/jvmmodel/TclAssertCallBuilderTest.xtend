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
		val assertionTestStep = parseAssertionTestStep('- assert variable == "test"')

		// when
		val genCode = assertCallBuilder.build(assertionTestStep.expression)
		val assertMethod=genCode.split(System.lineSeparator).last

		// then
		assertMethod.assertEquals('org.junit.Assert.assertEquals("test", variable);')
	}

	@Test
	def void testNotEqualsGen() {
		// given
		val assertionTestStep = parseAssertionTestStep('- assert variable != "test"')

		// when
		val genCode = assertCallBuilder.build(assertionTestStep.expression)
		val assertMethod=genCode.split(System.lineSeparator).last

		// then
		assertMethod.assertEquals('org.junit.Assert.assertNotEquals("test", variable);')
	}

	@Test
	def void testInEqualityGen() {
		// given
		val assertionTestStep = parseAssertionTestStep('- assert variable is not "test"')

		// when
		val genCode = assertCallBuilder.build(assertionTestStep.expression)
		val assertMethod=genCode.split(System.lineSeparator).last

		// then
		assertMethod.assertEquals('org.junit.Assert.assertNotEquals("test", variable);')
	}

	@Test
	def void testNotNullGen() {
		// given
		val assertionTestStep = parseAssertionTestStep('- assert variable')

		// when
		val genCode = assertCallBuilder.build(assertionTestStep.expression)
		val assertMethod=genCode.split(System.lineSeparator).last

		// then
		assertMethod.assertEquals('org.junit.Assert.assertNotNull(variable);')
	}

	@Test
	def void testNullGen() {
		// given
		val assertionTestStep = parseAssertionTestStep('- assert !variable')

		// when
		val genCode = assertCallBuilder.build(assertionTestStep.expression)
		val assertMethod=genCode.split(System.lineSeparator).last

		// then
		assertMethod.assertEquals('org.junit.Assert.assertNull(variable);')
	}

	@Test
	def void testIncompleteImpl() {
		// given
		val assertionTestStep = parseAssertionTestStep('- assert variable > "ohoh"')

		// when
		val genCode = assertCallBuilder.build(assertionTestStep.expression)
		val assertMethod=genCode.split(System.lineSeparator).last

		// then
		assertMethod.assertMatches('// TODO .*')
	}

	@Test
	def void testMatches() {
		// given
		val assertionTestStep = parseAssertionTestStep('- assert variable matches "ohoh"')

		// when
		val genCode = assertCallBuilder.build(assertionTestStep.expression)
		val assertMethod=genCode.split(System.lineSeparator).last

		// then
		assertMethod.assertEquals('org.junit.Assert.assertTrue(variable.matches("ohoh"));')
	}

	@Test
	def void testDoesNotMatch() {
		// given
		val assertionTestStep = parseAssertionTestStep('- assert variable does not match "ohoh"')

		// when
		val genCode = assertCallBuilder.build(assertionTestStep.expression)
		val assertMethod=genCode.split(System.lineSeparator).last

		// then
		assertMethod.assertEquals('org.junit.Assert.assertFalse(variable.matches("ohoh"));')
	}

	@Test
	def void testWithMapDereference() {
		// given
		val assertionTestStep = parseAssertionTestStep('- assert variable.key == "test"')

		// when
		val genCode = assertCallBuilder.build(assertionTestStep.expression)
		val assertMethod=genCode.split(System.lineSeparator).last

		// then
		assertMethod.assertEquals('org.junit.Assert.assertEquals("test", variable.get("key"));')
	}

	@Test
	def void testWithMapKeyAsString() {
		// given
		val assertionTestStep = parseAssertionTestStep('- assert variable."key with spaces" == "test"')

		// when
		val genCode = assertCallBuilder.build(assertionTestStep.expression)
		val assertMethod=genCode.split(System.lineSeparator).last

		// then
		assertMethod.assertEquals('org.junit.Assert.assertEquals("test", variable.get("key with spaces"));')
	}

	private def AssertionTestStep parseAssertionTestStep(CharSequence seq) {
		return seq.parse(grammarAccess.assertionTestStepRule, AssertionTestStep)
	}
	
	@Test
	def void testGeneratedComment() {
		// given
		val assertionTestStep = parseAssertionTestStep('- assert variable."key with spaces" == "test"')

		// when
		val genCode = assertCallBuilder.build(assertionTestStep.expression)
		val assertComment=genCode.split(System.lineSeparator).head

		// then
		assertComment.assertMatches('// - assert +variable."key with spaces" == "test"')
		
	}

}
