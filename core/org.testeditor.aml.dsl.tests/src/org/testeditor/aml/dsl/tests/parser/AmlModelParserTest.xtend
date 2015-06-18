package org.testeditor.aml.dsl.tests.parser

import org.junit.Test

/**
 * Parsing tests for {@link AmlModel}.
 */
class AmlModelParserTest extends AbstractParserTest {

	/**
	 * Test parsing a minimal model with only a package definition.
	 */
	@Test
	def void parseMinimal() {
		// Given
		val input = '''
			package com.example
		'''
		
		// When
		val model = parser.parse(input)
		
		// Then
		model => [
			assertNoErrors
			package.assertEquals("com.example")
		]
	}
	
	/**
	 * Test that elements of the AmlModel can be in any order.
	 */
	@Test
	def void parseSmallModel() {
		// Given
		val input = '''
			package com.example
			
			interaction type MyInteraction
			component MyComponent is MyComponentType
			component type MyComponentType
			component MyOtherComponent is MyComponentType
		'''
		
		// When
		val model = parser.parse(input)
		
		// Then
		model => [
			assertNoErrors
			components.assertSize(2)
			componentTypes.assertSingleElement
			interactionTypes.assertSingleElement
		]
	}

}