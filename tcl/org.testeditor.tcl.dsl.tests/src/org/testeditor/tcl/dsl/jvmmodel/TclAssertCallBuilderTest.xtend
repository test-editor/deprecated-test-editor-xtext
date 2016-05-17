package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.junit.Test
import org.testeditor.tml.AssertionTestStep
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest

class TclAssertCallBuilderTest extends AbstractParserTest {

	@Inject TclAssertCallBuilder assertCallBuilder

	@Test
	def void testEqualsGen() {
		'- assert variable == "test"' //
		.assertTclTestStepGeneratesCodeLine( //
		'org.junit.Assert.assertEquals("test", variable);')
	}

	@Test
	def void testNotEqualsGen() {
		'- assert variable != "test"' //
		.assertTclTestStepGeneratesCodeLine( //
		'org.junit.Assert.assertNotEquals("test", variable);')
	}

	@Test
	def void testInEqualityGen() {
		'- assert variable is not "test"' //
		.assertTclTestStepGeneratesCodeLine( //
		'org.junit.Assert.assertNotEquals("test", variable);')
	}

	@Test
	def void testNotNullGen() {
		'- assert variable' //
		.assertTclTestStepGeneratesCodeLine( //
		'org.junit.Assert.assertNotNull(variable);')
	}

	@Test
	def void testNullGen() {
		'- assert !variable' //
		.assertTclTestStepGeneratesCodeLine( //
		'org.junit.Assert.assertNull(variable);')
	}

	@Test
	def void testIncompleteImpl() {
		'- assert variable > "ohoh"'//
		.assertTclTestStepGeneratesMatchingComment(//
		'// TODO .*')
	}

	@Test
	def void testMatches() {
		'- assert variable matches "ohoh"' //
		.assertTclTestStepGeneratesCodeLine( //
		'org.junit.Assert.assertTrue(variable.matches("ohoh"));')
	}

	@Test
	def void testDoesNotMatch() {
		'- assert variable does not match "ohoh"' //
		.assertTclTestStepGeneratesCodeLine( //
		'org.junit.Assert.assertFalse(variable.matches("ohoh"));')
	}

	@Test
	def void testWithMapDereference() {
		'- assert variable.key == "test"' //
		.assertTclTestStepGeneratesCodeLine( //
		'org.junit.Assert.assertEquals("test", variable.get("key"));')
	}

	@Test
	def void testWithMapKeyAsString() {
		'- assert variable."key with spaces" == "test"' //
		.assertTclTestStepGeneratesCodeLine( //
		'org.junit.Assert.assertEquals("test", variable.get("key with spaces"));')
	}

	@Test
	def void testGeneratedComment() {
		'- assert variable."key with spaces" == "test"'//
		.assertTclTestStepGeneratesMatchingComment(//
		'// - assert variable."key with spaces" == "test"')
	}

	// parse the given char sequence as tcl AssertionTestStep
	private def AssertionTestStep parseAssertionTestStep(CharSequence seq) {
		return seq.parse(grammarAccess.assertionTestStepRule, AssertionTestStep)
	}

	// assert that the given tcl AssertionTestStep results in the generation of the expectedCode
	private def void assertTclTestStepGeneratesCodeLine(String testStep, String expectedCode) {
		// given
		val assertionTestStep = parseAssertionTestStep(testStep)

		// when
		val genCode = assertCallBuilder.build(assertionTestStep.expression)

		// then
		genCode.assertCodeLine(expectedCode)
	}

	// assert that the given tcl AssertionTestStep results in the generation of the expectedCommentPattern
	private def void assertTclTestStepGeneratesMatchingComment(String testStep, String expectedCommentPattern) {
		// given
		val assertionTestStep = parseAssertionTestStep(testStep)

		// when
		val genCode = assertCallBuilder.build(assertionTestStep.expression)

		// then
		genCode.assertMatchingCommentLine(expectedCommentPattern)
	}

	// assert that the generated code holds 2 lines of which the second is identical to expectedCode
	private def void assertCodeLine(String generatedCode, String expectedCode) {
		val generatedCodeLines = generatedCode.split(System.lineSeparator)
		assertSize(generatedCodeLines, 2, "expecting two generated code lines, a comment and the assertion call")
		val assertMethod = generatedCodeLines.last
		assertMethod.assertEquals(expectedCode)
	}

	// assert that the generated codes first line matches the expectedCommentPattern
	private def void assertMatchingCommentLine(String generatedCode, String expectedCommentPattern) {
		val assertMethod = generatedCode.split(System.lineSeparator).head
		assertMethod.assertMatches(expectedCommentPattern)
	}

}
