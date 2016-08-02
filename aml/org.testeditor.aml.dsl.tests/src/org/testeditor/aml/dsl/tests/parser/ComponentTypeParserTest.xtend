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
import org.junit.Before
import org.junit.Test
import org.testeditor.aml.ComponentType
import org.testeditor.dsl.common.testing.ResourceSetHelper

/**
 * Parsing tests for {@link ComponentType}.
 */
class ComponentTypeParserTest extends AbstractParserTest {
	
	@Inject extension ResourceSetHelper
	
	@Before
	def void setUp() {
		setUpResourceSet
	}

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
		#[withoutBrackets, withBrackets].map[parse(ComponentType, resourceSet)].forEach[
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
		val componentType = input.parse(ComponentType, resourceSet)
		
		// Then
		componentType => [
			assertNoErrors
			label.assertEquals("SWT Dialog")
		]
	}
	
}