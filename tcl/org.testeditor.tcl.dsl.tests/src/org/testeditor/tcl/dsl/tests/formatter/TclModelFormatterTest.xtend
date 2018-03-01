package org.testeditor.tcl.dsl.tests.formatter

import org.junit.Test

class TclModelFormatterTest extends AbstractTclFormatterTest {

	@Test
	def void formatLineBreaks() {
		assertFormatted [
			expectation = '''
				package com.example
				
				import org.testeditor
				import de.some
				
				require freq, breq
				
				# testCase
			'''

			toBeFormatted = '''
				package com.example	import org.testeditor
				import de.some
				require
				freq
				,
				breq
				#
				testCase
			'''
		]
	}

	@Test
	def void formatLineBreaksWithImplements() {
		assertFormatted [
			expectation = '''
				package com.example
				
				import org.testeditor
				import de.some
				
				require freq, breq
				
				# testCase implements SomeSpec
			'''

			toBeFormatted = '''
				package com.example	import org.testeditor
				import de.some
				require
				freq
				,
				breq
				#
				testCase
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
								
				# Test * Step
			'''
		]
	}

	@Test
	def void formatWhitespaces() {
		assertFormatted [
			expectation = '''
				package com.example
				
				import org.testeditor
				import de.some
				
				require freq, breq
				
				# testCase implements SomeSpec
			'''

			toBeFormatted = '''
				package com.example	import org.testeditor
				import de.some 
				         require            freq          ,            breq            #
				testCase   	implements      SomeSpec
			'''
		]
	}

	@Test
	def void formatLineBreaksTml() {
		assertFormatted [
			expectation = '''
				package com.example
				
				import org.testeditor
				import de.some
				
				# MacroCollection
			'''

			toBeFormatted = '''
				package com.example	import org.testeditor
				import de.some
				#
				MacroCollection
			'''
		]
	}

	@Test
	def void formatWhitespacesTml() {
		assertFormatted [
			expectation = '''
				package com.example
				
				import org.testeditor
				import de.some
				
				# MacroCollection
			'''

			toBeFormatted = '''
				package com.example	import org.testeditor
				import de.some
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
									
					# Test    «keyword»   :  Component: myComponent - sample setup
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
			toBeFormatted = expectation.toSingleLine
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
