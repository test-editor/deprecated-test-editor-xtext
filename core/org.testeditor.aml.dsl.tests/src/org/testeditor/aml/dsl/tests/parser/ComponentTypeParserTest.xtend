package org.testeditor.aml.dsl.tests.parser

import org.junit.Test
import org.testeditor.aml.model.ComponentType

/**
 * Parsing tests for {@link ComponentType}.
 */
class ComponentTypeParserTest extends AbstractParserTest {
	
	@Test
	def void parseMinimal() {
		// Given
		val withoutBrackets = '''
			component type MyComponentType
		'''
		val withBrackets = '''
			component type MyComponentType {
			}
		'''
		
		// When + Then
		#[withoutBrackets, withBrackets].map[parse(ComponentType)].forEach[
			assertNoErrors
			name.assertEquals("MyComponentType")
		]
	}
	
	@Test
	def void parseWithLabel() {
		// Given
		val input = '''
			component type SwtDialog {
				label = "SWT Dialog"
			}
		'''
		
		// When
		val componentType = input.parse(ComponentType)
		
		// Then
		componentType => [
			assertNoErrors
			label.assertEquals("SWT Dialog")
		]
	}
	
}