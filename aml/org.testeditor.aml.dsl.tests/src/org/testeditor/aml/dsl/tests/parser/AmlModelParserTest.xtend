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
import org.testeditor.aml.model.AmlModel

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