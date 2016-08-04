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
package org.testeditor.aml.dsl.tests.parser

import javax.inject.Inject
import org.junit.Test
import org.testeditor.aml.Component
import org.testeditor.dsl.common.testing.ResourceSetHelper

/**
 * Parsing tests for {@link Component}.
 */
class ComponentParserTest extends AbstractParserTest {
	
	val typeName = "Dialog"

	@Inject extension ResourceSetHelper
	
	@Test
	def void parseMinimal() {
		// Given
		val withoutBrackets = '''
			component MyDialog is «typeName»
		'''.addType
		val withBrackets = '''
			component MyDialog is «typeName» {
			}
		'''.addType

		// When + Then
		#[withoutBrackets, withBrackets].map[parse(Component, resourceSet)].forEach [
			assertNoErrors
			name.assertEquals("MyDialog")
			type.assertNotNull.name.assertEquals(typeName)
			isAbstract.assertFalse
			parents.assertEmpty
		]
	}
	
	@Test
	def void parseWithLabel() {
		// Given
		val input = '''
			component MyDialog is «typeName» {
				label = "Wonderful dialog"
			}
		'''.addType
		
		// When
		val component = input.parse(Component, resourceSet)
		
		// Then
		component => [
			assertNoErrors
			label.assertEquals("Wonderful dialog")
		]
	}
	
	@Test
	def void parseWithElements() {
		// Given
		val input = '''
			component MyDialog is «typeName» {
				element OkButton is Button
				element CancelButton is Button
			}
			element type Button
		'''.addType
		
		// When
		val component = input.parse(Component, resourceSet)
		
		// Then
		component => [
			assertNoErrors
			elements.size.assertEquals(2)
			elements.head.name.assertEquals("OkButton")
			elements.last.name.assertEquals("CancelButton")
		]
	}
	
	@Test
	def void parseWithAbstract() {
		// Given
		val input = '''
			abstract component MyDialog is «typeName»
		'''.addType
		
		// When
		val component = input.parse(Component, resourceSet)
		
		// Then
		component => [
			assertNoErrors
			isAbstract.assertTrue
		]
	}
	
	@Test
	def void parseWithParents() {
		// Given
		val parent1 = "MyComposite1"
		val parent2 = "MyComposite2"
		val input = '''
			component MyDialog is «typeName» includes «parent1», «parent2»
			
			component type Composite
			component «parent1» is Composite
			component «parent2» is Composite
		'''.addType
		
		// When
		val component = input.parse(Component, resourceSet)
		
		// Then
		component => [
			assertNoErrors
			name.assertEquals("MyDialog")
			parents.assertSize(2)
			parents.head.name.assertEquals(parent1)
			parents.last.name.assertEquals(parent2)
		]
	}
	
	@Test
	def void parseWithInheritedType() {
		// Given
		val componentName = "MyDialog"
		val input = '''
			component «componentName» includes AbstractDialog
			abstract component AbstractDialog is «typeName»
		'''.addType
		
		// When
		val component = input.parse(Component, resourceSet)
		component => [
			assertNoErrors
			name.assertEquals(componentName)
		]
	}
	
	protected def addType(CharSequence input) '''
		«input»
		component type «typeName»
	'''
	
}