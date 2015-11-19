package org.testeditor.tsl.dsl.parser

import org.junit.Test

class SimpleTslParserTest extends AbstractParserTest {

	@Test
	def void parseEmptyModel() {
		'''
			package com.example
		'''.parse[
			assertNoErrors
			package.assertEquals('com.example')
		]
	}

	@Test
	def void parseSimpleSpecification() {
		'''
			package com.example
			
			* Start the famous greetings application.
		'''.parse[
			steps.assertSingleElement => [
				contents.map[value].join(' ').assertEquals('Start the famous greetings application')
			]
		]
	}
	
	@Test
	def void parseSpecificationWithVariable() {
		'''
			package com.example
			
			* Send greetings "Hello World" to the world.
		'''.parse[
			steps.assertSingleElement => [
				contents.map[value].join(' ').assertEquals('Send greetings Hello World to the world')
			]
		]
	}

}