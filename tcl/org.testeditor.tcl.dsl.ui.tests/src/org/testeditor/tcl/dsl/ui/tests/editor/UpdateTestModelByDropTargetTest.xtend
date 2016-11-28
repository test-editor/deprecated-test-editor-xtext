package org.testeditor.tcl.dsl.ui.tests.editor

import org.junit.Test

class UpdateTestModelByDropTargetTest extends AbstractTestModelByDropTargetTest {

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
			
				Mask: GreetingApplication
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

		setTclModel(testCase)
		val target = getTestStep(testCase, "GreetingApplication", null)

		executeTest(droppedTestStep, target, testCase, codeToBeInserted)

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
				- Start application "path"
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

		setTclModel(testCase)
		val dropTarget = getTestStep(testCase, "GreetingApplication", 0)

		executeTest(droppedTestStep, dropTarget, testCase, codeToBeInserted)
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
				- Start application "path"
			-->INSERT HERE
				- Stop application
				- Start application "path"
				- Stop application
				- Wait "miliSeconds" ms
			
				Mask: GreetingApplication2
				- Starte2 application "path"
		'''

		setTclModel(testCase)
		val dropTarget = getTestStep(testCase, "GreetingApplication", 2)

		executeTest(droppedTestStep, dropTarget, testCase, codeToBeInserted)
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
				- Wait "miliSeconds" ms
			-->INSERT HERE
			
				Mask: GreetingApplication2
				- Starte2 application "path"
		'''

		setTclModel(testCase)
		val target = getTestStep(testCase, "GreetingApplication", 6)

		executeTest(droppedTestStep, target, testCase, codeToBeInserted)
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
				- Start application "path"
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

		setTclModel(testCase)
		val target = getTestStep(testCase, "GreetingApplication", 0)

		executeTest(droppedTestStep, target, testCase, codeToBeInserted)
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
				- Wait "miliSeconds" ms
			
			-->INSERT HERE
			
				Mask: GreetingApplication2
				- Starte2 application "path"
		'''

		setTclModel(testCase)
		val target = getTestStep(testCase, "GreetingApplication", 6)

		executeTest(droppedTestStep, target, testCase, codeToBeInserted)
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

		setTclModel(testCase)
		val target = getTestStep(testCase, "GreetingApplication", null)

		executeTest(droppedTestStep, target, testCase, codeToBeInserted)
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
			package SwingDemo
			
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

		setTclModel(testCase)
		val target = tclModel

		executeTest(droppedTestStep, target, testCase, codeToBeInserted)
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
			
			* zwei
			
			-->INSERT HERE
				Mask: GreetingApplication
				- Start application "path"
		'''.toString().replaceAll("\r\n", "\n")

		setTclModel(testCase)
		val target = tclModel.test.steps.last
		executeTest(droppedTestStep, target, testCase, codeToBeInserted)
	}

}
