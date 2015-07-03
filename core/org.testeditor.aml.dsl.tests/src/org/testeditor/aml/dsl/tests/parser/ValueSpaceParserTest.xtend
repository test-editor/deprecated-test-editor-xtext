package org.testeditor.aml.dsl.tests.parser

import org.junit.Test
import org.testeditor.aml.model.IntegerRange
import org.testeditor.aml.model.StringLiterals

class ValueSpaceParserTest extends AbstractParserTest {
	
	@Test
	def void parseStringLiterals() {
		// Given
		val stringLiterals = '''
			value-space Colors = #[ "Red", "Green", "Blue" ]
		'''
		
		// When + Then
		stringLiterals.parse(StringLiterals) => [
			assertNoErrors
			values.assertSize(3)
			values.get(0).assertEquals("Red")
			values.get(1).assertEquals("Green")
			values.get(2).assertEquals("Blue")
		]
	}
	
	@Test
	def void parseIntegerRange() {
		// Given
		val input = '''
			value-space DayInMonth = 1 .. 31
		'''
		
		// When + Then
		input.parse(IntegerRange) => [
			assertNoErrors
			from.assertEquals(1)
			to.assertEquals(31)
		]
	}
	
}