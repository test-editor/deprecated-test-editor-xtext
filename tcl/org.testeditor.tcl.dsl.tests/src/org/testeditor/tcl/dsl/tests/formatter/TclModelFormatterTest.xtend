package org.testeditor.tcl.dsl.tests.formatter

import org.junit.Test

class TclModelFormatterTest extends AbstractTclFormatterTest {

	@Test
	def void formatLineBreaks() {
		formatterTester.assertFormatted [
			expectation = '''
				package com.example
				
				import a.b.c
				import c.d.e
				
				require freq, breq
				
				# testCase
				
				* specification
				Component: some
				
				* next spec
			'''

			toBeFormatted = '''
				package com.example	import a.b.c
				import c.d.e
				require
				freq
				,
				breq
				#
				testCase
				*
				specification
				Component
				:
				some
				*
				next
				spec
			'''
		]
	}

	@Test
	def void formatWhitespaces() {
		formatterTester.assertFormatted [
			expectation = '''
				package com.example
				
				import a.b.c
				import c.d.e
				
				require freq, breq
				
				# testCase
				
				* specification
				Component: some
				
				* next spec
			'''

			toBeFormatted = '''
				package com.example	import a.b.c
				import c.d.e 
				         require            freq          ,            breq            #
				testCase   			*   			specification				Component				:				some			
				   *				next				spec
			'''
		]
	}
}
