package org.testeditor.tcl.dsl.tests.formatter

import org.junit.Ignore
import org.junit.Test

@Ignore("formatting is broken because of newline elements in the grammar. formatter is currently not in use (by the web frontend)!")
class TclModelFormatterTest extends AbstractTclFormatterTest {

	@Test
	def void formatLineBreaks() {
		assertFormatted [
			expectation = '''
				package com.example
				
				import a.b.c
				import c.d.e
				
				require freq, breq
				
				# TestCase
			'''

			toBeFormatted = '''
				package com.example	import a.b.c
				import c.d.e
				require
				freq,
				breq
				# TestCase
			'''
		]
	}

	@Test
	def void formatLineBreaksWithImplements() {
		assertFormatted [
			expectation = '''
				package com.example
				
				import a.b.c
				import c.d.e
				
				require freq, breq
				
				# testCase implements SomeSpec
			'''

			toBeFormatted = '''
				package com.example	import a.b.c
				import c.d.e
				require
				freq,
				breq
				# testCase
				implements
				SomeSpec
			'''
		]
	}

	@Test
	def void twoLinesBetweenNameAndSteps() {
		assertFormatted[
			expectation = '''
				package com.example
				
				# Test
				
				* Step
			'''
			toBeFormatted = '''
				package com.example
								
				# Test 
				* Step
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
				
				require freq, breq
				
				# testCase implements SomeSpec
			'''

			toBeFormatted = '''
				package com.example	import a.b.c
				import c.d.e 
				         require            freq          ,            breq            
				# testCase   	implements      SomeSpec
			'''
		]
	}

	@Test
	def void formatLineBreaksTml() {
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
				# MacroCollection
			'''
		]
	}

	@Test
	def void formatWhitespacesTml() {
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

	@Test
	def void formatSetupAndCleanup() {
		val keywords = #['Setup', 'Cleanup']
		keywords.forEach [ keyword |
			assertFormatted [
				expectation = '''
					package com.example
					
					# Test
					
					«keyword»:
					
						Component: myComponent
						- sample setup
				'''
				toBeFormatted = '''
					package com.example
									
					# Test    
					«keyword»   :  
					Component: myComponent 
					- sample setup
				'''
			]
		]
	}

	@Test
	def void formatConfigReference() {
		assertFormatted[
			expectation = '''
				package com.example
				
				# Test
				
				config MyConfig
			'''.trim
			toBeFormatted = '''
				package com.example				
				# Test				
				config MyConfig
			'''.trim
		]
	}

	@Test
	def void allowEmptyLinesBetweenTestSteps() {
		assertFormatted[
			toBeFormatted = '''
				package com.example
				
				# Test
				
				* The whole test
				
					Component: myComponent
				
					// given
					- here is the given
				
					// when
					- here is the when
				
					// then
					- here are the assertions
			'''.trim
		]
	}

}
