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
	@Test
	def void formatLineBreaksTml() {
		assertFormatted [
			expectation = prefix + '''
				* spec
				
					Component: dummyComponent
					- step withspaces "string" with <ele> and @some.
					- next step
				'''

			toBeFormatted = prefix + '''
				*
				spec
				Component
				:
				dummyComponent
				-
				step
				withspaces
				"string"
				with
				<ele>
				and
				@
				some
				.
				-
				next
				step
			'''
		]
	}

	@Test
	def void formatWhitespacesTml() {
		assertFormatted [
			expectation = prefix + '''
				* spec

					Component: component
					- step withspaces "string" with <ele> and @some.
					- next step
			'''

			toBeFormatted = prefix + '''
				   *      	spec      Component :    	component   - 
				step    			withspaces		    	"string"				with				<ele>				and
				@
				some				.				-  next    step
			'''
		]
	}
}
