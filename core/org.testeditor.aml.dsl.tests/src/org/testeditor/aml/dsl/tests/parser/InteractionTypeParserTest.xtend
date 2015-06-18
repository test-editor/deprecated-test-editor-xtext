package org.testeditor.aml.dsl.tests.parser

import org.junit.Test
import org.testeditor.aml.model.InteractionType

/**
 * Parsing tests for {@link InteractionType}.
 */
class InteractionTypeParserTest extends AbstractParserTest {
	
	@Test
	def void parseMinimal() {
		// Given
		val withoutBrackets = '''
			interaction type MyInteractionType
		'''
		val withBrackets = '''
			interaction type MyInteractionType {
			}
		'''
		
		// When + Then
		#[withoutBrackets, withBrackets].map[parse(InteractionType)].forEach[
			assertNoErrors
			name.assertEquals("MyInteractionType")
		]
	}
	
	@Test
	def void parseWithLabel() {
		// Given
		val input = '''
			interaction type MyInteractionType {
				label = "Do something"
			}
		'''
		
		// When
		val interactionType = input.parse(InteractionType)
		
		// Then
		interactionType => [
			assertNoErrors
			label.assertEquals("Do something")
		]
	}
	
	@Test
	def void parseWithTemplate() {
		// Given
		val template = "Put ${value} in ${element}."
		val input = '''
			interaction type MyInteractionType {
				template = "«template»"
			}
		'''
		
		// When
		val interactionType = input.parse(InteractionType)
		
		// Then
		interactionType => [
			assertNoErrors
			template.assertEquals(template)
		]
	}
	
}