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
package org.testeditor.aml.dsl.generator

import javax.inject.Inject
import org.junit.Test

/**
 * Tests for {@link AllActionGroupsGenerator}.
 */
class AllActionGroupsGeneratorTest extends AbstractGeneratorTest {

	@Inject AllActionGroupsGenerator generator

	/**
	 * Test the generation of all action groups with various scenarios at once since its
	 * quite complex to create a model programatically.
	 */
	@Test
	def void testGenerateAllActionGroups() {
		// Given
		val model = createAmlModel => [
			// Test a component with interactions
			components += createComponent("MyApplication") => [
				type = createComponentType("application") => [
					interactionTypes += createInteractionType("start-application", null)
					interactionTypes += createInteractionType("stop-application", null)
				]
			]
			
			// Test an component with elements
			components += createComponent("MyDialog") => [
				// don't set a type
				val setValueTemplateVariable = createTemplateVariable("value")
				val setInteraction = createInteractionType("set") => [
					template = factory.createTemplate => [
						contents += createTemplateText("Set the value")
						contents += setValueTemplateVariable
						contents += createTemplateText("on element")
						contents += createTemplateVariable("element")
					]
				]
				val textType = createElementType("Text") => [
					interactionTypes += setInteraction
					interactionTypes += createInteractionType("get")
				]
				elements += createElement("street", textType)
				elements += createElement("city", textType) => [
					locator = "locator::city"
					label = "Stadt"
					valueSpaceAssignments += factory.createValueSpaceAssignment => [
						variable = setValueTemplateVariable
						valueSpace = factory.createIntegerRange => [
							from = 2
							to = 6
						]
					]
				]
			]
		]

		// When
		val result = generator.generateAllActionGroups(model)

		// Then
		val expected = '''
			<ActionGroup name="MyApplication">
				<action technicalBindingType="start-application" />
				<action technicalBindingType="stop-application" />
			</ActionGroup>
			<ActionGroup name="MyDialog">
				<action technicalBindingType="set">
					<actionName locator="">street</actionName>
				</action>
				<action technicalBindingType="get">
					<actionName locator="">street</actionName>
				</action>
				<action technicalBindingType="set">
					<actionName locator="locator::city">Stadt</actionName>
					<argument id="value">
						<value>2</value>
						<value>3</value>
						<value>4</value>
						<value>5</value>
						<value>6</value>
					</argument>
				</action>
				<action technicalBindingType="get">
					<actionName locator="locator::city">Stadt</actionName>
				</action>
			</ActionGroup>
		'''.generateXml
		result.assertEquals(expected)
	}
	
	/**
	 * Test the generation of values for integer ranges
	 */
	@Test
	def void testGenerateValuesIntegerRange() {
		// Given
		val singleInt = factory.createIntegerRange => [
			from = -5
			to = -5
		]
		val upRange = factory.createIntegerRange => [
			from = -1
			to = 2
		]
		val downRange = factory.createIntegerRange => [
			from = 5
			to = 2
		]
		
		// When
		val singleIntResult = generator.generateValues(singleInt)
		val upRangeResult = generator.generateValues(upRange)
		val downRangeResult = generator.generateValues(downRange)
		
		// Then
		singleIntResult.toString.assertEquals('''
			<value>-5</value>
		'''.toString)
		upRangeResult.toString.assertEquals('''
			<value>-1</value>
			<value>0</value>
			<value>1</value>
			<value>2</value>
		'''.toString)
		downRangeResult.toString.assertEquals('''
			<value>5</value>
			<value>4</value>
			<value>3</value>
			<value>2</value>
		'''.toString)
	}
	
	/**
	 * Tests the generation of values of StringLiterals.
	 */
	@Test
	def void testGenerateValuesStringLiterals() {
		// Given
		val singleStringLiteral = factory.createStringLiterals => [
			values += "Hello"
		]
		val doubleStringsLiteral = factory.createStringLiterals => [
			values += "Hello,"
			values += "world"
		]
		
		// When
		val singleResult = generator.generateValues(singleStringLiteral)
		val doubleResult = generator.generateValues(doubleStringsLiteral)
		
		// Then
		singleResult.toString.assertEquals('''
			<value>Hello</value>
		'''.toString)
		doubleResult.toString.assertEquals('''
			<value>Hello,</value>
			<value>world</value>
		'''.toString)
	} 

	/**
	 * Test the generation for an empty model.
	 */
	@Test
	def void testEmptyModel() {
		// Given
		val model = createAmlModel

		// When
		val result = generator.generateAllActionGroups(model)

		// Then
		val expected = generateXml(null)
		result.assertEquals(expected)
	}

	/**
	 * Test the generation for two simple components just to make sure
	 * the corresponding method is called.
	 */
	@Test
	def void testSimpleComponents() {
		// Given
		val model = createAmlModel => [
			components += createComponent("A")
			components += createComponent("B") => [
				label = "B Component"
			]
		]

		// When
		val result = generator.generateAllActionGroups(model)

		// Then
		val expected = generateXml('''
			<ActionGroup name="A">
			</ActionGroup>
			<ActionGroup name="B Component">
			</ActionGroup>
		''')
		result.assertEquals(expected)
	}

	/**
	 * Generate the expected XML with the passed contents
	 */
	private def String generateXml(CharSequence contents) '''
		«AllActionGroupsGenerator.XML_HEADER»
		«AllActionGroupsGenerator.OPEN_TAG»
			«contents»
		«AllActionGroupsGenerator.CLOSE_TAG»
	'''

}