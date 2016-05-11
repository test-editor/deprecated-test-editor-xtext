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
package org.testeditor.tcl.dsl.tests.parser

import org.junit.Test
import org.testeditor.tml.AEComparison
import org.testeditor.tml.AENullCheck
import org.testeditor.tml.AEStringConstant
import org.testeditor.tml.AEVariableReference
import org.testeditor.tml.AssertionTestStep
import org.testeditor.tml.ComparatorMatches
import org.testeditor.tml.ComponentTestStepContext
import org.testeditor.tml.MacroTestStepContext
import org.testeditor.tml.StepContentElement
import org.testeditor.tml.TestStep
import org.testeditor.tml.TestStepWithAssignment
import org.testeditor.tml.TmlPackage
import org.testeditor.tsl.StepContentVariable

import static extension org.eclipse.xtext.nodemodel.util.NodeModelUtils.*

class TclModelParserTest extends AbstractParserTest {
	
	@Test
	def void parseMinimal() {
		// given
		val input = '''
			package com.example
		'''
		
		// when
		val model = parser.parse(input)
		
		// then
		model.package.assertEquals('com.example')
	}
	
	@Test
	def void parseSimpleSpecificationStep() {
		// given
		val input = '''
			package com.example
			
			# MyTest
			* Start the famous
			greetings application.
		'''
		
		// when
		val test = parse(input)
		
		// then
		test.name.assertEquals('MyTest')
		test.steps.assertSingleElement => [
			contents.restoreString.assertEquals('Start the famous greetings application')
		]
	}
	
	@Test
	def void parseSpecificationStepWithVariable() {
		// given
		val input = '''
			package com.example
			
			# Test
			* send greetings "Hello World" to the world.
		'''
		
		// when
		val test = parse(input)
		
		// then
		test.steps.assertSingleElement => [
			contents.restoreString.assertEquals('send greetings "Hello World" to the world')
			contents.get(2).assertInstanceOf(StepContentVariable) => [
				value.assertEquals('Hello World')
			]
		]		
	}
	
	@Test
	def void parseTestContextWithSteps() {
		// given
		val input = '''
			package com.example
			
			# Test
			* Start the famous greetings application
				Mask: GreetingsApplication
				- starte Anwendung "org.testeditor.swing.exammple.Greetings"
				- gebe in <Eingabefeld> den Wert "Hello World" ein.
		'''
		
		// when
		val test = parse(input)
		
		// then
		test.steps.assertSingleElement => [
			contexts.assertSingleElement.assertInstanceOf(ComponentTestStepContext) => [
				val componentNode = findNodesForFeature(TmlPackage.Literals.COMPONENT_TEST_STEP_CONTEXT__COMPONENT).assertSingleElement
				componentNode.text.assertEquals('GreetingsApplication')
				steps.assertSize(2)
				steps.get(0) => [
					contents.restoreString.assertEquals('starte Anwendung "org.testeditor.swing.exammple.Greetings"')	
				]
				steps.get(1) => [
					contents.restoreString.assertEquals('gebe in <Eingabefeld> den Wert "Hello World" ein')
				]
			]
		]
	}
	
	@Test
	def void parseEmptyComponentElementReference() {
		// given
		val input = '''
			package com.example
			
			# Test
			* Dummy step
				Mask: Demo
				- <> < 	> <
				>
		'''
		
		// when
		val test = parse(input)
		
		// then
		test.steps.assertSingleElement.contexts.assertSingleElement.assertInstanceOf(ComponentTestStepContext) => [
			val emptyReferences = steps.assertSingleElement.contents.assertSize(3)
			emptyReferences.forEach[
				assertInstanceOf(StepContentElement)
				value.assertNull
			]
		]
	}
	
	@Test
	def void parseTestStepWithVariableAssignmentSteps() {
		// given
		val input = '''
			package com.example
			
			# Test
			* Start
				Mask: Demo
				- hello = Lese den Text von <Input>
		'''
		
		// when
		val test = parse(input)
		
		// then
		test.steps.assertSingleElement => [
			contexts.assertSingleElement.assertInstanceOf(ComponentTestStepContext) => [
				steps.assertSingleElement.assertInstanceOf(TestStepWithAssignment) => [
					variableName.assertEquals('hello')
					contents.restoreString.assertEquals('Lese den Text von <Input>')
				]
			]
		]
	}

	@Test
	def void parseTestStepAssertionWOComparator() {
		// given
		val input = '''
			package com.example
			
			# Test
			* Start using some keywords like is matches does not match
			  Mask: Demo
			  - assert hello
		'''

		// when
		val test = parse(input)

		// then
		test.steps.assertSingleElement => [
			contexts.assertSingleElement.assertInstanceOf(ComponentTestStepContext) => [
				steps.assertSingleElement.assertInstanceOf(AssertionTestStep) => [
					expression.assertInstanceOf(AENullCheck) => [
						negated.assertFalse
						varReference.name.assertEquals("hello")
					]
				]
			]
		]
	}

	@Test
	def void parseTestStepAssertion() {
		// given
		val input = '''
			package com.example
			
			# Test
			* Start using some keywords like is matches does not match
			  Mask: Demo
			  - assert hello does    not match ".*AAABBB.*"
		'''

		// when
		val test = parse(input)

		// then
		test.steps.assertSingleElement => [
			contexts.assertSingleElement.assertInstanceOf(ComponentTestStepContext) => [
				steps.assertSingleElement.assertInstanceOf(AssertionTestStep) => [
					expression.assertInstanceOf(AEComparison) => [
						left.assertInstanceOf(AEVariableReference) => [name.assertEquals("hello")]
						comparator.assertInstanceOf(ComparatorMatches) => [negated.assertTrue]
						right.assertInstanceOf(AEComparison) => [
							left.assertInstanceOf(AEStringConstant) => [string.assertEquals(".*AAABBB.*")]
							comparator.assertNull
						]
					]
				]
			]
		]
	}

	@Test
	def void parseMacroTestStep(){
		// given
		val input = '''
			package com.example

			# Test
			* Do some complex step
			  Macro: MyMacroFile
			  - template execute with "param" as a and "param2"
		'''

		// when
		val test = parse(input)

		// then
		test.steps.assertSingleElement => [
			contexts.assertSingleElement.assertInstanceOf(MacroTestStepContext) => [
				step.assertInstanceOf(TestStep) => [
					contents.restoreString.assertMatches('template execute with "param" as a and "param2"')
				]
			]
		]
	}

}
