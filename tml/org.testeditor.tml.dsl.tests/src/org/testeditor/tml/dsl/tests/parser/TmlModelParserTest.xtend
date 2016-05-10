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
		val model = parser.parse(input)
		
		// then
		model.package.assertEquals('com.example')
	}
	
	
}