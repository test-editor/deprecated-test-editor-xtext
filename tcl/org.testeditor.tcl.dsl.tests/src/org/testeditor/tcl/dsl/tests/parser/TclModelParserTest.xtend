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
import org.testeditor.tcl.AEComparison
import org.testeditor.tcl.AENullOrBoolCheck
import org.testeditor.tcl.AEStringConstant
import org.testeditor.tcl.AssertionTestStep
import org.testeditor.tcl.ComparatorMatches
import org.testeditor.tcl.ComplexVariableReference
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.TclPackage
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.TestStepWithAssignment
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
		val model = parseHelper.parse(input)
		
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
		val tcl = parse(input)
		
		// then
		tcl.test.name.assertEquals('MyTest')
		tcl.test.steps.assertSingleElement => [
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
		val test = parse(input).test
		
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
		val test = parse(input).test
		
		// then
		test.steps.assertSingleElement => [
			contexts.assertSingleElement.assertInstanceOf(ComponentTestStepContext) => [
				val componentNode = findNodesForFeature(TclPackage.Literals.COMPONENT_TEST_STEP_CONTEXT__COMPONENT).assertSingleElement
				componentNode.text.trim.assertEquals('GreetingsApplication')
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
		val test = parse(input).test
		
		// then
		test.steps.assertSingleElement.contexts.assertSingleElement.assertInstanceOf(ComponentTestStepContext) => [
			val emptyReferences = steps.assertSingleElement.contents.assertSize(3)
			emptyReferences.forEach[
				assertInstanceOf(StepContentElement) => [
					value.assertNull
				]
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
		val test = parse(input).test
		
		// then
		test.steps.assertSingleElement => [
			contexts.assertSingleElement.assertInstanceOf(ComponentTestStepContext) => [
				steps.assertSingleElement.assertInstanceOf(TestStepWithAssignment) => [
					variable.name.assertEquals('hello')
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
			  - hello = some
			  - assert hello
		'''

		// when
		val test = parse(input).test

		// then
		test.steps.assertSingleElement => [
			contexts.assertSingleElement.assertInstanceOf(ComponentTestStepContext) => [
				steps.assertSize(2).get(1).assertInstanceOf(AssertionTestStep) => [
					expression.assertInstanceOf(AENullOrBoolCheck) => [
						isNegated.assertFalse
						varReference.simpleVariable.name.assertEquals("hello")
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
			  - hello = some
			  - assert hello does    not match ".*AAABBB.*"
		'''

		// when
		val test = parse(input).test

		// then
		test.steps.assertSingleElement => [
			contexts.assertSingleElement.assertInstanceOf(ComponentTestStepContext) => [
				steps.assertSize(2).get(1).assertInstanceOf(AssertionTestStep) => [
					expression.assertInstanceOf(AEComparison) => [
						left.assertInstanceOf(ComplexVariableReference) => [simpleVariable.name.assertEquals("hello")]
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
	def void parseMacroTestStep() {
		// given
		val input = '''
			package com.example

			# Test
			* Do some complex step
			  Macro: MyMacroFile
			  - template execute with "param" as a and "param2"
		'''

		// when
		val test = parse(input).test

		// then
		test.steps.assertSingleElement => [
			contexts.assertSingleElement.assertInstanceOf(MacroTestStepContext) => [
				step.assertInstanceOf(TestStep) => [
					contents.restoreString.assertMatches('template execute with "param" as a and "param2"')
				]
			]
		]
	}

	@Test
	def void parseWithMultiVariableDereference() {
		// given
		val input = '''
			package com.example

			# MyMacroCollection

			## MacroStartWith
			template = "start with" ${startparam}
			Component: MyComponent
			- put @startparam into <other>

			// uses macro defined above
			## MacroUseWith
			template = "use macro with" ${useparam}
			Macro: MyMacroCollection
			- start with @useparam
		'''

		// when
		val model = parse(input).assertNoSyntaxErrors

		// then
		model.package.assertEquals('com.example')
	}

	@Test
	def void parseSetup() {
		// given
		val input = '''
			package com.example
			
			# Test
			
			Setup:
				Component: Demo
		'''

		// when
		val test = parse(input).test

		// then
		test.assertNoSyntaxErrors
		test.setup.assertNotNull
		test.setup.contexts.assertSingleElement
	}

	@Test
	def void parseCleanup() {
		// given
		val input = '''
			package com.example
			
			# Test
			
			Cleanup:
				Component: Demo
		'''

		// when
		val test = parse(input).test

		// then
		test.assertNoSyntaxErrors
		test.cleanup.assertNotNull
		test.cleanup.contexts.assertSingleElement
	}

	@Test
	def void parseSetupAndCleanupBeforeSpecificationSteps() {
		// given
		val input = '''
			package com.example
			
			# Test
			
			Setup:
				Component: MySetupComponent
			
			Cleanup:
				Component: MyCleanupComponent
			
			* step1
		'''

		// when
		val test = parse(input).test

		// then
		test.assertNoSyntaxErrors
		test.setup.assertNotNull
		test.cleanup.assertNotNull
		test.steps.assertSingleElement
	}

	@Test
	def void parseSetupAndCleanupAfterSpecificationSteps() {
		// given
		val input = '''
			package com.example
			
			# Test
			
			* step1
			
			Cleanup:
				Component: MyCleanupComponent
			
			Setup:
				Component: MySetupComponent
		'''

		// when
		val test = parse(input).test

		// then
		test.assertNoSyntaxErrors
		test.setup.assertNotNull
		test.cleanup.assertNotNull
		test.steps.assertSingleElement
	}

}
