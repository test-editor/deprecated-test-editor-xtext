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
package org.testeditor.tcl.dsl.ui.tests.editor

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.resource.SaveOptions
import org.junit.Test
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.ui.editor.TclModelDragAndDropUpdater
import org.testeditor.tcl.TestStepWithAssignment

class TclModelDragAndDropUpdaterIntegrationTest extends AbstractTclModelDragAndDropUpdaterTest {

	@Inject TclModelDragAndDropUpdater classUnderTest
	
	@Test
	def void dropTestStepOnFirstTestStepContext() {

		// given
		val droppedTestStep = createDroppedTestStepContext("GreetingApplication", "test")
		val codeToBeInserted = '- Inserted step "path"'

		// then		
		val testCase = '''
			package SwingDemo
			
			# SwingDemoEins
			
			*
			
				Mask: GreetingApplication           // <-- drop target
			-->INSERT HERE
				- Start application "path"
				- Stop application
				- Start application "path"
				- Stop application
				- Start application "path"
				- Stop application
				- Wait "miliSeconds" ms
			
				Mask: GreetingApplication2
				- Starte2 application "path"
		'''

		val tclModel = parseTclModel(testCase)
		val dropTarget = tclModel.getComponentTestStepContext("GreetingApplication")

		tclModel.executeTest(droppedTestStep, dropTarget, testCase, codeToBeInserted)
	}

	@Test
	def void dropTestStepOnFirstTestStep() {

		// given
		val codeToBeInserted = '- Inserted step "path"'
		val droppedTestStep = createDroppedTestStepContext("GreetingApplication", "test")

		// then			
		val testCase = '''
			package SwingDemo
			
			# SwingDemoEins
			
			*
			
				Mask: GreetingApplication
				- Start application "path"       // <-- drop target
			-->INSERT HERE
				- Stop application
				- Start application "path"
				- Stop application
				- Start application "path"
				- Stop application
				- Wait "miliSeconds" ms
			
				Mask: GreetingApplication2
				- Starte2 application "path"
		'''

		val tclModel = parseTclModel(testCase)
		val dropTarget = tclModel.getTestStep("GreetingApplication", 0)

		tclModel.executeTest(droppedTestStep, dropTarget, testCase, codeToBeInserted)
	}

	@Test
	def void dropTestSetpOnThirdTestStep() {

		// given
		val droppedTestStep = createDroppedTestStepContext("GreetingApplication", "test")
		val codeToBeInserted = '- Inserted step "path"'

		// then			
		val testCase = '''
			package SwingDemo
			
			# SwingDemoEins
			
			*
			
				Mask: GreetingApplication
				- Start application "path"
				- Stop application
				- Start application "path"         // <-- drop target
			-->INSERT HERE
				- Stop application
				- Start application "path"
				- Stop application
				- Wait "miliSeconds" ms
			
				Mask: GreetingApplication2
				- Starte2 application "path"
		'''

		val tclModel = parseTclModel(testCase)
		val dropTarget = tclModel.getTestStep("GreetingApplication", 2)

		tclModel.executeTest(droppedTestStep, dropTarget, testCase, codeToBeInserted)
	}

	@Test
	def void dropTestStepOnLastTestStep() {

		// given
		val droppedTestStep = createDroppedTestStepContext("GreetingApplication", "test")
		val codeToBeInserted = '- Inserted step "path"'

		// then			
		val testCase = '''
			package SwingDemo
			
			# SwingDemoEins
			
			*
			
				Mask: GreetingApplication
				- Start application "path"
				- Stop application
				- Start application "path"
				- Stop application
				- Start application "path"
				- Stop application
				- Wait "miliSeconds" ms           // <-- drop target
			-->INSERT HERE
			
				Mask: GreetingApplication2
				- Starte2 application "path"
		'''

		val tclModel = parseTclModel(testCase)
		val dropTarget = tclModel.getTestStep("GreetingApplication", 6)

		tclModel.executeTest(droppedTestStep, dropTarget, testCase, codeToBeInserted)
	}

	@Test
	def void dropTestStepWithDifferenComponentOnFirstTestStep() {

		// given
		val droppedTestStep = createDroppedTestStepContext("GreetingApplication2", "Input", "insertIntoTextField")
		val codeToBeInserted = '''			
		
		Mask: GreetingApplication2
		- Insert "text" into field <Input>
		
		Mask: GreetingApplication'''

		// then			
		val testCase = '''
			package SwingDemo
			
			# SwingDemoEins
			
			*
			
				Mask: GreetingApplication
				- Start application "path"        // <-- drop target
			-->INSERT HERE
				- Stop application
				- Start application "path"
				- Stop application
				- Start application "path"
				- Stop application
				- Wait "miliSeconds" ms
			
				Mask: GreetingApplication2
				- Starte2 application "path"
		'''

		val tclModel = parseTclModel(testCase)
		val dropTarget = tclModel.getTestStep("GreetingApplication", 0)

		tclModel.executeTest(droppedTestStep, dropTarget, testCase, codeToBeInserted)
	}

	@Test
	def void dropTestStepWithDifferenComponentOnLastTestStep() {

		// given
		val droppedTestStep = createDroppedTestStepContext("GreetingApplication2", "Input", "insertIntoTextField")
		val codeToBeInserted = '''
		Mask: GreetingApplication2
		- Insert "text" into field <Input>'''

		// then			
		val testCase = '''
			package SwingDemo
			
			# SwingDemoEins
			
			*
			
				Mask: GreetingApplication
				- Start application "path"
				- Stop application
				- Start application "path"
				- Stop application
				- Start application "path"
				- Stop application
				- Wait "miliSeconds" ms          // <-- drop target
			
			-->INSERT HERE
			
				Mask: GreetingApplication2
				- Starte2 application "path"
		'''

		val tclModel = parseTclModel(testCase)
		val dropTarget = tclModel.getTestStep("GreetingApplication", 6)

		tclModel.executeTest(droppedTestStep, dropTarget, testCase, codeToBeInserted)
	}

	@Test
	def void dropTestStepWithDifferenComponentOnComponent() {

		// given
		val droppedTestStep = createDroppedTestStepContext("GreetingApplication2", "Input", "insertIntoTextField")
		val codeToBeInserted = '''
			Mask: GreetingApplication2
			- Insert "text" into field <Input>
		'''

		// then			
		val testCase = '''
			package SwingDemo
			
			# SwingDemoEins
			
			*
			
			-->INSERT HERE
				Mask: GreetingApplication        // <-- drop target
				- Start application "path"
				- Stop application
				- Start application "path"
				- Stop application
				- Start application "path"
				- Stop application
				- Wait "miliSeconds" ms
			
				Mask: GreetingApplication2
				- Starte2 application "path"
		'''

		val tclModel = parseTclModel(testCase)
		val dropTarget = tclModel.getComponentTestStepContext("GreetingApplication")

		tclModel.executeTest(droppedTestStep, dropTarget, testCase, codeToBeInserted)
	}

	@Test
	def void dropTestStepOnTclModel() {

		// given
		var droppedTestStep = createDroppedTestStepContext("GreetingApplication2", "Input", "insertIntoTextField")
		val codeToBeInserted = '''
			Mask: GreetingApplication2
			- Insert "text" into field <Input>
		'''

		// then			
		val testCase = '''
			package SwingDemo                   // <-- drop target
			
			# SwingDemoEins
			
			*
			
			-->INSERT HERE
				Mask: GreetingApplication
				- Start application "path"
				- Stop application
				- Start application "path"
				- Stop application
				- Start application "path"
				- Stop application
				- Wait "miliSeconds" ms
			
				Mask: GreetingApplication2
				- Starte2 application "path"
		'''

		val tclModel = parseTclModel(testCase)
		val dropTarget = tclModel

		tclModel.executeTest(droppedTestStep, dropTarget, testCase, codeToBeInserted)
	}

	@Test
	def void dropTestStepOnTestSpecification() {
		// given
		val droppedTestStep = createDroppedTestStepContext("GreetingApplication2", "Input", "insertIntoTextField")
		val codeToBeInserted = '''			
			Mask: GreetingApplication2
			- Insert "text" into field <Input>
		'''
		// then			
		val testCase = '''
			package SwingDemo
			
			# SwingDemoEins
			
			* eins
			
				Mask: GreetingApplication
				- Start application "path"
			
			* zwei                              // <-- drop target
			
			-->INSERT HERE
				Mask: GreetingApplication
				- Start application "path"
		'''.toString.replace('\r', '')

		val tclModel = parseTclModel(testCase)
		val dropTarget = tclModel.test.steps.last

		tclModel.executeTest(droppedTestStep, dropTarget, testCase, codeToBeInserted)
	}

	@Test
	def void dropTestStepOnMacroCall() {
		// given
		val droppedTestStep = createDroppedTestStepContext("GreetingApplication2", "Input", "insertIntoTextField")
		val codeToBeInserted = '''			
			Mask: GreetingApplication2
			- Insert "text" into field <Input>
			'''
		
		// then			
		val testCase = '''
			package SwingDemo
			
			# SwingDemoEins
			
			*
			
				Mask: GreetingApplication
				- Start application "path"

				Macro: MacroCall
				- Call to macro    // <-- drop target
			
			-->INSERT HERE
				Mask: GreetingApplication2
				- Starte2 application "path"
		'''.toString.replace('\r', '')

		val tclModel = parseTclModel(testCase)
		val dropTarget = tclModel.getMarcoCall("MacroCall", 0)

		tclModel.executeTest(droppedTestStep, dropTarget, testCase, codeToBeInserted)
	}
	@Test
	def void dropTestStepOnMacroCallsAndSplitCallSection() {
		// given
		val droppedTestStep = createDroppedTestStepContext("GreetingApplication2", "Input", "insertIntoTextField")
		val codeToBeInserted = '''
			
			Mask: GreetingApplication2
			- Insert "text" into field <Input>
			
			Macro: MacroCall'''
		
		// then			
		val testCase = '''
			package SwingDemo
			
			# SwingDemoEins
			
			*
			
				Mask: GreetingApplication
				- Start application "path"

				Macro: MacroCall
				- Call to macro    // <-- drop target
			-->INSERT HERE
				- Call to macro
			
				Mask: GreetingApplication2
				- Starte2 application "path"
		'''.toString.replace('\r', '')

		val tclModel = parseTclModel(testCase)
		val dropTarget = tclModel.getMarcoCall("MacroCall", 0)

		tclModel.executeTest(droppedTestStep, dropTarget, testCase, codeToBeInserted)
	}

	@Test
	def void dropTestStepAtEndOnATestCaseEndingWithAMacroCall() {
		// given
		val droppedTestStep = createDroppedTestStepContext("GreetingApplication2", "Input", "insertIntoTextField")
		val codeToBeInserted = '''
			
			Mask: GreetingApplication2
			- Insert "text" into field <Input>'''
		
		// then			
		val testCase = '''
			package SwingDemo
			
			# SwingDemoEins
			
			*
			
				Mask: GreetingApplication
				- Start application "path"

				Macro: MacroCall
				- Call to macro    // <-- drop target
			-->INSERT HERE'''.toString.replace('\r', '')

		val tclModel = parseTclModel(testCase)
		val dropTarget = tclModel.getMarcoCall("MacroCall", 0)

		tclModel.executeTest(droppedTestStep, dropTarget, testCase, codeToBeInserted)
	}
	@Test
	def void dropTestStepVariableName() {

		// given
		val codeToBeInserted = '- Inserted step "path"'
		val droppedTestStep = createDroppedTestStepContext("GreetingApplication", "test")

		// then			
		val testCase = '''
			package SwingDemo
			
			# SwingDemoEins
			
			*
			
				Mask: GreetingApplication
				- Start application "path"
				- test = Wait "miliSeconds" ms // <-- drop target
			-->INSERT HERE
			
				Mask: GreetingApplication2
				- Starte2 application "path"
		'''

		val tclModel = parseTclModel(testCase)
		val testStepWithAssignment = tclModel.getTestStep("GreetingApplication", 1)  as TestStepWithAssignment
		val dropTarget = testStepWithAssignment.variable

		tclModel.executeTest(droppedTestStep, dropTarget, testCase, codeToBeInserted)
	}
	

	def private executeTest(TclModel tclModel, ComponentTestStepContext newTestStepContext, EObject dropTarget,
		String testCase, String insertedCode) {
		val expectedTestCase = testCase.replaceAll('-->INSERT HERE', insertedCode.indent(1)).replace('\r', '').replaceAll(' *// <-- drop target','')

		classUnderTest.updateTestModel(tclModel, dropTarget, newTestStepContext, newArrayList)
		val actualTestCase = tclSerializer.serialize(tclModel, SaveOptions.newBuilder.format.options).replace('\r', '')
		actualTestCase.assertEquals(expectedTestCase)
	}
	
	def private TclModel parseTclModel(String testCase) {
		val tclToParse = testCase.replaceAll('-->INSERT HERE.*(\r?\n)?', '').replaceAll(' *// <-- drop target', '')
		parseTcl(tclToParse)
	}
	
}
