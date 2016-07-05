package org.testeditor.tml.dsl.tests.formatter

import org.junit.Test

class TmlModelFormatterTest extends AbstractTmlFormatterTest {

	@Test
	def void formatLineBreaks() {
		assertFormatted [
			expectation = '''
				package com.example
				
				import a.b.c
				import c.d.e
				
				# MacroCollection
				
			'''

			toBeFormatted = '''
				package com.example	import a.b.c
				import c.d.e
				#
				MacroCollection
			'''
		]
	}

	@Test
	def void formatWhitespaces() {
		assertFormatted [
			expectation = '''
				package com.example
				
				import a.b.c
				import c.d.e
				
				# MacroCollection
				
			'''

			toBeFormatted = '''
				package com.example	import a.b.c
				import c.d.e 
				    #    MacroCollection
			'''
		]
	}
}
