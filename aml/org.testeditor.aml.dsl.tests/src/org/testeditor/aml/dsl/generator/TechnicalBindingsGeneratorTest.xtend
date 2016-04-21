/*******************************************************************************
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
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
package org.testeditor.aml.dsl.generator

import javax.inject.Inject
import org.junit.Test

/**
 * Tests for {@link TechnicalBindingsGenerator}.
 */
class TechnicalBindingsGeneratorTest extends AbstractGeneratorTest {

	@Inject TechnicalBindingsGenerator generator

	/**
	 * Test the generation for empty interaction types
	 */
	@Test
	def void testGenerateTechnicalBinding() {
		// Given
		val interactionType = createInteractionType("theName", "theLabel") => [
			template = factory.createTemplate => [
				contents += createTemplateText("Some text")
				contents += createTemplateVariable("element")
				contents += createTemplateVariable("value")
				contents += createTemplateText(".")
				contents += createTemplateVariable("")
			]
		]

		// When
		val result = generator.generateTechnicalBinding(interactionType).toString

		// Then
		val expected = '''
			<TechnicalBindingType id="theName" name="theLabel">
				<actionPart position="1" type="TEXT" value="Some text" />
				<actionPart position="2" type="ACTION_NAME" />
				<actionPart position="3" type="ARGUMENT" id="value" />
				<actionPart position="4" type="TEXT" value="." />
				<actionPart position="5" type="ARGUMENT" />
			</TechnicalBindingType>
		'''
		result.assertEquals(expected)
	}

	/**
	 * Test the generation for an empty model.
	 */
	@Test
	def void testEmptyModel() {
		// Given
		val model = createAmlModel

		// When
		val result = generator.generateTechnicalBindings(model)

		// Then
		val expected = generateXml(null)
		result.assertEquals(expected)
	}

	/**
	 * Test the generation for two simple interactions just to make sure
	 * the corresponding method is called.
	 */
	@Test
	def void testSimpleInteractions() {
		// Given
		val model = createAmlModel => [
			interactionTypes += createInteractionType("get", "") // set no label to verify the name is taken
			interactionTypes += createInteractionType("set", "Set")
		]

		// When
		val result = generator.generateTechnicalBindings(model)

		// Then
		val contents = '''
			<TechnicalBindingType id="get" name="get">
			</TechnicalBindingType>
			<TechnicalBindingType id="set" name="Set">
			</TechnicalBindingType>
		'''
		val expected = contents.generateXml
		result.assertEquals(expected)
	}
	
	/**
	 * Generate the expected XML with the passed contents
	 */
	private def String generateXml(CharSequence contents) '''
		«TechnicalBindingsGenerator.XML_HEADER»
		«TechnicalBindingsGenerator.OPEN_TAG»
			«contents»
		«TechnicalBindingsGenerator.CLOSE_TAG»
	'''

}