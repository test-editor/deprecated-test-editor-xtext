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

import org.junit.Test
import org.testeditor.aml.ComponentElementType

/**
 * Parsing tests for {@link ComponentElementType}.
 */
class ComponentElementTypeParserTest extends AbstractParserTest {

	@Test
	def void parseMinimal() {
		// Given
		val withoutBrackets = '''
			element type MyElementType
		'''
		val withBrackets = '''
			element type MyElementType {
			}
		'''

		// When + Then
		#[withoutBrackets, withBrackets].map[parseAmlWithUniquePackage(ComponentElementType)].forEach [
			assertNoErrors
			name.assertEquals("MyElementType")
			label.assertNull
			interactionTypes.assertEmpty
		]
	}

	@Test
	def void parseWithLabel() {
		// Given
		val input = '''
			element type Button {
				label = "SWT Button"
			}
		'''

		// When
		val elementType = input.parseAmlWithStdPackage(ComponentElementType)

		// Then
		elementType => [
			assertNoErrors
			label.assertEquals("SWT Button")
		]
	}
	
	@Test
	def void parseWithInteractionTypes() {
		// Given
		val input = '''
			element type Text {
				interactions = getValue, setValue
			}
			interaction type getValue
			interaction type setValue
		'''
		
		// When
		val elementType = input.parseAmlWithStdPackage(ComponentElementType)
		
		// Then
		elementType => [
			assertNoErrors
			interactionTypes.assertSize(2)
			interactionTypes.head.name.assertEquals("getValue")
			interactionTypes.last.name.assertEquals("setValue")
		]
	}

}