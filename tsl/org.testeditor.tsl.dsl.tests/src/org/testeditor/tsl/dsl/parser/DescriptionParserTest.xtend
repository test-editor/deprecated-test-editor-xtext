package org.testeditor.tsl.dsl.parser

import org.junit.Test

class DescriptionParserTest extends AbstractParserTest {

	@Test
	def void parseSimpleDescription() {
		// given
		val description = 'simple description'

		// expect
		description.parseDescription.assertEquals(description)
	}

	@Test
	def void parseDescription() {
		// given
		val description = '''
			Feature: Sample feature
				As a developer I want the sample feature in order
				to be able to do awesome stuff. See more on http://www.example.org - or
				at <a href="http://www.example.com">example.com</a>.
		'''.toString.trim

		// expect
		description.parseDescription.assertEquals(description)
	}

	/**
	 * Helper method for parsing the description within the TSL.
	 */
	private def String parseDescription(String description) {
		// given
		val tsl = '''
			package com.example
			
			# Test
			
			«description»
		'''

		// when
		val tslModel = tsl.parse

		// then
		tslModel.assertNoSyntaxErrors
		return tslModel.specification.description
	}

}
