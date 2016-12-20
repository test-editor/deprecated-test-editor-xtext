package org.testeditor.tcl.dsl.ui.tests.editor

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.junit.Test
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.ui.editor.TclModelDragAndDropUpdater

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
		val target = tclModel.getTestStepContext("GreetingApplication")

		tclModel.executeTest(droppedTestStep, target, testCase, codeToBeInserted)
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
		val target = tclModel.getTestStep("GreetingApplication", 6)

		tclModel.executeTest(droppedTestStep, target, testCase, codeToBeInserted)
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
		val target = tclModel.getTestStep("GreetingApplication", 0)

		tclModel.executeTest(droppedTestStep, target, testCase, codeToBeInserted)
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
		val target = tclModel.getTestStep("GreetingApplication", 6)

		tclModel.executeTest(droppedTestStep, target, testCase, codeToBeInserted)
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
		val target = tclModel.getTestStepContext("GreetingApplication")

		tclModel.executeTest(droppedTestStep, target, testCase, codeToBeInserted)
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
		val target = tclModel

		tclModel.executeTest(droppedTestStep, target, testCase, codeToBeInserted)
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
		val target = tclModel.test.steps.last

		tclModel.executeTest(droppedTestStep, target, testCase, codeToBeInserted)
	}

	def private executeTest(TclModel tclModel, ComponentTestStepContext newTestStepContext, EObject dropTarget,
		String testCase, String insertedCode) {
		val expectedTestCase = testCase.replaceAll('-->INSERT HERE', insertedCode.indent(1)).replace('\r', '').replaceAll(' *// <-- drop target','')

		classUnderTest.updateTestModel(tclModel.test, dropTarget, newTestStepContext, newArrayList)
		val actualTestCase = serializer.serialize(tclModel).replace('\r', '')
		actualTestCase.assertEquals(expectedTestCase)
	}
	
	def private TclModel parseTclModel(String testCase) {
		parserHelper.parseTcl(testCase.replace('-->INSERT HERE', ''))
	}

	

}
