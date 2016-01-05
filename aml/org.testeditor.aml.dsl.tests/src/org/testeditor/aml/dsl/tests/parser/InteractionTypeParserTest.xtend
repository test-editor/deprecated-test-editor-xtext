/*******************************************************************************
 * Copyright (c) 2012 - 2015 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 *******************************************************************************/
package org.testeditor.aml.dsl.tests.parser

import org.junit.Test
import org.testeditor.aml.model.InteractionType
import org.testeditor.aml.model.TemplateText
import org.testeditor.aml.model.TemplateVariable

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
	def void parseWithStringOnlyTemplate() {
		// Given
		val templateText = "test"
		val input = '''
			interaction type MyInteractionType {
				template = "«templateText»"
			}
		'''
		
		// When
		val interactionType = input.parse(InteractionType)
		
		// Then
		interactionType => [
			assertNoErrors
			val head = template.contents.head.assertInstanceOf(TemplateText)
			head.value.assertEquals(templateText)
		]
	}
	
	@Test
	def void parseWithComplexTemplate() {
		// Given
		val input = '''
			interaction type MyInteractionType {
				template = "Put" ${value} "into field" ${element} "."
			}
		'''
		
		// When
		val interactionType = input.parse(InteractionType)
		
		// Then
		interactionType => [
			assertNoErrors
			template.contents => [
				get(0).assertInstanceOf(TemplateText).value.assertEquals("Put")
				get(1).assertInstanceOf(TemplateVariable).name.assertEquals("value")
				get(2).assertInstanceOf(TemplateText).value.assertEquals("into field")
				get(3).assertInstanceOf(TemplateVariable).name.assertEquals("element")
				get(4).assertInstanceOf(TemplateText).value.assertEquals(".")
			]
		]
	}
	
}