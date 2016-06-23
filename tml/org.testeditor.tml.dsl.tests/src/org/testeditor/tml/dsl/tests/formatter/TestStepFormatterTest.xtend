package org.testeditor.tml.dsl.tests.formatter

import org.junit.Test

class TestStepFormatterTest extends AbstractTmlFormatterTest {

	val prefix = '''
		package com.example
		
		# Test
		
		## Some
		template = "aha"
		
			Component: some
		'''

	@Test
	def void formatLineBreaks() {
		assertFormatted [
			expectation = prefix + '''
				«''»	- step withspaces "string" with <ele> and @some.
				«''»	- next step
			'''

			toBeFormatted = prefix + '''
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
	def void formatWhitespaces() {
		assertFormatted [
			expectation = prefix + '''
				«''»	- step withspaces "string" with <ele> and @some.
				«''»	- next step
			'''

			toBeFormatted = prefix + '''
				   -
				step    			withspaces		    	"string"				with				<ele>				and
				@
				some				.				-  next    step
			'''
		]
	}
}
