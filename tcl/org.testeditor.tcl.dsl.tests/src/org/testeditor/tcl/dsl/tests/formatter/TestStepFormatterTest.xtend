package org.testeditor.tcl.dsl.tests.formatter

import org.junit.Test

class TestStepFormatterTest extends AbstractTclFormatterTest {

	val prefix = '''
		package com.example
		
		import a.b.c
		import c.d.e
		
		require freq, breq
		
		# testCase
		
	'''

	@Test
	def void formatLineBreaks() {
		assertFormatted [
			expectation = prefix + '''
				* specification
				
					Component: some
				
					Mask: other
				
				* next spec
			'''

			toBeFormatted = prefix + '''
				*
				specification
				Component
				:
				some
				Mask
				:
				other
				*
				next
				spec
			'''
		]
	}

	@Test
	def void formatWhitespaces() {
		assertFormatted [
			expectation = prefix + '''
				* specification
				
					Component: some
				
					Mask: other
				
				* next spec
			'''

			toBeFormatted = prefix + '''
				*   			specification				Component				:				some			
				  Mask     :      other   *				next				spec
			'''
		]
	}
}
