package org.testeditor.aml.dsl.tests.formatter

import org.junit.Test

class ValueSpaceFormatterTest extends AbstractFormatterTest {
	
	@Test
	def void formatStringLiterals() {
		assertFormatted[
			expectation = '''
				value-space Colors = #[ "Red", "Green", "Blue" ]
			'''
			toBeFormatted = '''
				value-space Colors = #[ "Red" ,"Green"   , "Blue"]
			'''
		]
	}
	
	@Test
	def void formatIntegerRange() {
		assertFormatted[
			expectation = '''
				value-space DayInMonth = 1 .. 31
			'''
			toBeFormatted = '''
				value-space   DayInMonth=1..31
			'''
		]
	}
	
}