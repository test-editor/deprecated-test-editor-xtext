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
	def void parseUtf8Specname() {
		// given
		val tsl = '''
			package com.example
			
			# ÖttoIsÜberμm
		'''
		
		// when
		val tslModel = tsl.parseTsl('ÖttoIsÜberμm.tsl')
		
		// then
		tslModel => [
			assertNoSyntaxErrors
			specification.name.assertEquals('ÖttoIsÜberμm')
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
	def void parseUnicodeLine() {
		// given
		val tsl = '''
			package testeditor.rest
			
			# JobCreation
			
			* Simple spec with a * in it
			* တက်စတ တက်စတ. တက်စတ@öttö.de. တက်စတ
			* !@#$%*^&()-_=+`~\|][}{;:''""<>,./? with ξεσκεπάζω την ξεσκεπάζω την Sævör grét áðan því úlpan var ónýt.
			* he\'s not very cooperative 'param'
			* he\"s not ccop. right? "param" and more
		'''
		// when
		val tslModel = tsl.parseTsl 
		
		// then (unicode characters are parsed as single characters, use node model to get original formatting)
		tslModel => [
			assertNoSyntaxErrors
			specification.steps.assertSize(5) => [
				get(0).contents.restoreString.assertEquals('Simple spec with a * in it')
				get(1).contents.restoreString.assertEquals('တ က ် စ တ တ က ် စ တ . တ က ် စ တ @ öttö . de . တ က ် စ တ')
				get(2).contents.restoreString.assertEquals('! @ # $ % * ^ & ( ) - _ = + ` ~ \\ | ] [ } { ; : "" "" < > , . / ? with ξεσκεπάζω την ξεσκεπάζω την Sævör grét áðan því úlpan var ónýt .')
				get(3).contents.restoreString.assertEquals("he \\' s not very cooperative \"param\"")
				get(4).contents.restoreString.assertEquals("he \\\" s not ccop . right ? \"param\" and more")
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
				get(1).contents.restoreString.assertEquals('The result of 5 * 3 is 15 .')
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
				contents.restoreString.assertEquals('Send greetings "Hello World" to the world .')
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

		// expect tsl parsed with default package (null)
		tsl.parseTsl.package.assertNull
	}
	
	@Test
	def void multilineSpecifications() {
		// given
		val tsl = '''
			package com.example
			
			# Test
			
			* One specification step including *
			  that spans over multiple
			  lines an includes £µ Ähnlich
			  
			  empty lines in between
			
			* and another simple spec step "with"
			  : "variable" * and description
			'''

		// when 
		tsl.parseTsl => [
			
			//  then
			assertNoSyntaxErrors
			specification.steps => [
				assertSize(2)
				get(0).contents.restoreString.assertEquals('One specification step including * that spans over multiple lines an includes £ µ Ähnlich empty lines in between')
				get(1).contents.restoreString.assertEquals('and another simple spec step "with" : "variable" * and description')
			]
		]
	}
	
	@Test
	def void multilineSpecWithComments() {
		// given
		val tsl = '''
			package com.example
			
			  // some comment
			  	/* and
			  	   a
			  	   multiline comment */
			
			# Test
			
			          Dies ist der erklaerende	http://some.site and Beschreibungs
			Text 
			// gehoert nicht mehr zur Beschreibung

			* One specification step including *
			  that spans over multiple and references
			  lines an includes £µ Ähnlich
			  
			  empty lines in between
			  
				// between to specs
				
			/*
			* commented spec step
			*/
				
			* and another simple spec step "with"
			  : "variable" * and description
			  
			  /* after a spec 
			     */
			'''

		// when 
		tsl.parseTsl => [
			
			//  then
			assertNoSyntaxErrors
			specification.description.assertEquals('Dies ist der erklaerende	http://some.site and Beschreibungs\nText')
			specification.steps => [
				assertSize(2)
				get(0).contents.restoreString.assertEquals('One specification step including * that spans over multiple and references lines an includes £ µ Ähnlich empty lines in between')
				get(1).contents.restoreString.assertEquals('and another simple spec step "with" : "variable" * and description')
			]
		]
	}
}
