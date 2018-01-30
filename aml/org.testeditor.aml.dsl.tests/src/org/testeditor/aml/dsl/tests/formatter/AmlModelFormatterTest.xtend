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
package org.testeditor.aml.dsl.tests.formatter

import org.junit.Test
import org.testeditor.aml.AmlModel
import org.testeditor.aml.ModelElement

/**
 * General formatting tests for {@link AmlModel}, not specific
 * to a certain subtype of {@link ModelElement}.
 */
class AmlModelFormatterTest extends AbstractFormatterTest {

	@Test
	def void formatElementsAndBrackets() {
		formatterTester.assertFormatted [
			expectation = '''
				package com.example
				
				component type NoBrackets
				
				component type EmptyBrackets {
				}
				
				component type ContentWithin {
					label = "Hello"
				}
			'''

			toBeFormatted = '''
				package com.example	component type NoBrackets
					 component type EmptyBrackets{}
				component type ContentWithin{ label = "Hello"}
			'''
		]
	}

	@Test
	def void formatSpaces() {
		assertFormatted[
			expectation = '''
				component type Dialog
				
				interaction type MyInteractionType {
				}
				
				component MyDialog is Dialog {
					label = "myLabel"
				}
			'''
			toBeFormatted = '''
				component    type  	Dialog	
				 interaction 	type  MyInteractionType   { }
				component MyDialog    is	Dialog {  label  = 	"myLabel" }
			'''
		]
	}

	@Test
	def void formatImports() {
		assertFormatted[
			expectation = '''
				import selenide.*
				import selenide.LocatorStrategy
				
				component type Test
			'''
			toBeFormatted = expectation
		]
		assertFormatted[
			expectation = '''
				import selenide.*
				import selenide.LocatorStrategy
				
				component type Test
			'''
			toBeFormatted = '''
				import selenide.*
					import selenide.LocatorStrategy
					
				component type Test
			'''
		]
	}

}
