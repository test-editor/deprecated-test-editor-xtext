package org.testeditor.tml.dsl.tests.parser

import org.junit.Test

class TmlModelParserTest extends AbstractParserTest {
	
	@Test
	def void parseMinimal() {
		// given
		val input = '''
			package com.example
		'''
		
		// when
		val model = parseHelper.parse(input)
		
		// then
		model.package.assertEquals('com.example')
	}
	
	


	@Test
	def void parseWithMultiVariableDereference() {
		// given
		val input = '''
			package com.example

			# MyMacroCollection

			template = "start with" ${startparam}
			Component: MyComponent
			- put @startparam into <other>

			template = "use macro with" ${useparam}
			Macro: MyMacroCollection
			- start with @useparam
		'''

		// when
		val model = parseHelper.parse(input)

		// then
		model.package.assertEquals('com.example')
	}

}