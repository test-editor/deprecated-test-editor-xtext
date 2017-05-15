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
package org.testeditor.tsl.dsl.parser

import javax.inject.Inject
import org.junit.Test
import org.testeditor.tsl.StepContentVariable
import org.testeditor.tsl.util.TslModelUtil

class SimpleTslParserTest extends AbstractParserTest {

	@Inject extension TslModelUtil

	@Test
	def void parseEmptyModelWithPackage() {
		// given
		val tsl = '''
			package com.example
		'''

		// expect
		tsl.parseTsl => [
			package.assertEquals('com.example')
			specification.assertNull
		]
	}

	@Test
	def void parseEmptyModel() {
		// given
		val tsl = ''

		// expect
		tsl.parseTsl => [
			package.assertNull
			specification.assertNull
		]
	}

	@Test
	def void parseEmptySpecification() {
		// given
		val tsl = '''
			package com.example
			
			# Test
		'''

		// expect
		tsl.parseTsl => [
			assertNoSyntaxErrors
			specification.name.assertEquals('Test')
		]
	}

	@Test
	def void parseSimpleSpecificationStep() {
		// given
		val tsl = '''
			package com.example
			
			# Test
			* Simple spec
		'''

		// expect
		tsl.parseTsl => [
			specification.steps.assertSingleElement => [
				contents.restoreString.assertEquals('Simple spec')
			]
		]
	}

	@Test
	def void parseSpecificationStepWithStar() {
		// given
		val tsl = '''
			package testeditor.rest
			
			# JobCreation
			
			* Simple spec with a * in it
			* The result of 5 * 3 is 15.
		'''

		// expect
		tsl.parseTsl => [
			assertNoSyntaxErrors
			specification.steps.assertSize(2) => [
				get(0).contents.restoreString.assertEquals('Simple spec with a * in it')
				get(1).contents.restoreString.assertEquals('The result of 5 * 3 is 15')
			]
		]
	}

	@Test
	def void parseMultipleSpecificationSteps() {
		// given
		val tsl = '''
			package com.example
			
			# Test
			
			* First step
			
			
			* Second step
			* Third step
			
				* Fourth step
				 * Fifth step
				 	* Sixt step
		'''

		// expect
		tsl.parseTsl => [
			assertNoSyntaxErrors
			specification.steps => [
				assertSize(6)
				get(0).contents.restoreString.assertEquals('First step')
				get(1).contents.restoreString.assertEquals('Second step')
				get(2).contents.restoreString.assertEquals('Third step')
				get(3).contents.restoreString.assertEquals('Fourth step')
				get(4).contents.restoreString.assertEquals('Fifth step')
				get(5).contents.restoreString.assertEquals('Sixt step')
			]
		]
	}

	@Test
	def void parseSpecificationStepWithVariable() {
		// given
		val tsl = '''
			package com.example
			
			# Test
			
			* Send greetings "Hello World" to the world.
		'''

		// expect
		tsl.parseTsl => [
			specification.steps.assertSingleElement => [
				contents.filter(StepContentVariable).assertSingleElement => [
					value.assertEquals('Hello World')
				]
				contents.restoreString.assertEquals('Send greetings "Hello World" to the world')
			]
		]
	}

	@Test
	def void parseDescriptionAndSpecificationStep() {
		// given
		val tslWithoutNewLines = '''
			package com.example
			
			# Test
			This is a sample description.
			* Step 1
			* Step 2
		'''
		val tslWithSingleNewLines = '''
			package com.example
						
			# Test
			
			This is a sample description.
			
			* Step 1
			* Step 2
		'''
		val tslWithMultipleNewLines = '''
			package com.example
			
			# Test
			
			
			
			This is a sample description.
			
			
			* Step 1
			* Step 2
				
		'''

		// expect
		#[tslWithoutNewLines, tslWithSingleNewLines, tslWithMultipleNewLines].forEach [
			parseTsl => [
				specification.description.assertEquals('This is a sample description.')
				specification.steps => [
					assertSize(2)
					get(0).contents.restoreString.assertEquals('Step 1')
					get(1).contents.restoreString.assertEquals('Step 2')
				]
			]
		]
	}

	@Test
	def void missingPackage() {
		// given
		val tsl = ""

		// expect tsl paresd with default package (null)
		tsl.parseTsl.package.assertNull
	}

}
