package org.testeditor.tcl.dsl.tests

import com.google.inject.Provider
import javax.inject.Inject
import org.eclipse.xtext.junit4.formatter.FormatterTestRequest
import org.eclipse.xtext.junit4.formatter.FormatterTester
import org.junit.Test

class TclModelFormatterTest extends AbstractTclTest {

	@Inject Provider<FormatterTestRequest> formatterRequestProvider
	@Inject protected FormatterTester formatterTester

	@Test
	def void formatLineBreaks() {
		formatterTester.assertFormatted [
			expectation = '''
				package com.example

				import a.b.c
				import c.d.e

				# testCase

				* specification
				Component: some
				- step withspaces "string" with <ele> and @some.

				* next spec
				Mask: oh
				- step
			'''

			toBeFormatted = '''
				package com.example	import a.b.c
				import c.d.e
				#
				testCase
				*
				specification
				Component
				:
				some
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
				*
				next
				spec
				Mask
				:
				oh
				-
				step
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

				# testCase

				* specification
				Component: some
				- step withspaces "string" with <ele> and @some.

				* next spec
				Mask: oh
				- step
			'''

			toBeFormatted = '''
				package com.example	import a.b.c
				import c.d.e 
				# 				testCase				*				specification				Component				:				some				-
				step				withspaces				"string"				with				<ele>				and
				@
				some				.				*				next				spec				Mask
				:				oh				-				step
			'''
		]
	}
}
