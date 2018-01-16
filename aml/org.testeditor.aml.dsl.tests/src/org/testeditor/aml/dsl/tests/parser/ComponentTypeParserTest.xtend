/*******************************************************************************
 * Copyright (c) 2012 - 2018 Signal Iduna Corporation and others.
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
import org.testeditor.aml.ComponentType

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
		#[withoutBrackets, withBrackets].map[parseAmlWithUniquePackage(ComponentType)].forEach[
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
		val componentType = input.parseAmlWithStdPackage(ComponentType)
		
		// Then
		componentType => [
			assertNoErrors
			label.assertEquals("SWT Dialog")
		]
	}
	
}