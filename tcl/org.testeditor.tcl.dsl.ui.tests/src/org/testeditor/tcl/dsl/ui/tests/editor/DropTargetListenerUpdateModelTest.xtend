package org.testeditor.tcl.dsl.ui.tests.editor

import com.google.common.base.Strings
import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.serializer.ISerializer
import org.junit.Before
import org.junit.Test
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.dsl.common.testing.AbstractTest
import org.testeditor.dsl.common.testing.DslParseHelper
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.TclStandaloneSetup
import org.testeditor.tcl.dsl.ui.editor.DropTargetXtextEditorListener
import com.google.common.io.ByteStreams

class DropTargetListenerUpdateModelTest extends AbstractTest {


	@Inject DropTargetXtextEditorListener listener

	@Inject protected extension DslParseHelper parserHelper
	var ISerializer serializer
	var AmlModel amlModel = null
	var TclModel tclModel = null

	@Before
	def void parseAmlModel() {
		var byte[] encoded = ByteStreams.toByteArray(this.getClass().getClassLoader().getResourceAsStream("test.aml"))
		amlModel = parserHelper.parseAml(new String(encoded));

		val tclInjector = (new TclStandaloneSetup).createInjectorAndDoEMFRegistration
		serializer = tclInjector.getInstance(ISerializer)
	}

	@Test
	def void testDropTestStepOnFirstTestStepContext() {

		val codeToBeInserted = '- Inserted step "path"'
		
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

		var target = getTestStep(testCase, "GreetingApplication", null)
		var droppedTestStep = createDroppedTestStepContext("GreetingApplication", "test")

		executeTest(droppedTestStep, target, testCase, codeToBeInserted)

	}
	

	@Test
	def void testDropTestStepOnFirstTestStep() {

		val codeToBeInserted = '- Inserted step "path"'

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

		var dropTarget = getTestStep(testCase, "GreetingApplication", 0)
		var droppedTestStep = createDroppedTestStepContext("GreetingApplication", "test")

		executeTest(droppedTestStep, dropTarget, testCase, codeToBeInserted)
	}

	@Test
	def void testDropTestSetpOnThirdTestStep() {

		val codeToBeInserted = '- Inserted step "path"'

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

		var dropTarget = getTestStep(testCase, "GreetingApplication", 2)
		var droppedTestStep = createDroppedTestStepContext("GreetingApplication", "test")

		executeTest(droppedTestStep, dropTarget, testCase, codeToBeInserted)
	}

	@Test
	def void testDropTestStepOnLastTestStep() {

		val codeToBeInserted = '- Inserted step "path"'

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

		var target = getTestStep(testCase, "GreetingApplication", 6)
		var droppedTestStep = createDroppedTestStepContext("GreetingApplication", "test")

		executeTest(droppedTestStep, target, testCase, codeToBeInserted)
	}

	@Test
	def void testDropTestStepWithDifferenComponentOnFirstTestStep() {

		val codeToBeInserted = '''			
			
			Mask: GreetingApplication2
			- Insert "text" into field <Input>
			
			Mask: GreetingApplication''' 

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

		var target = getTestStep(testCase, "GreetingApplication", 0)
		var droppedTestStep = createDroppedTestStepContext("GreetingApplication2", "Input", "insertIntoTextField")
		
		
		executeTest(droppedTestStep, target, testCase, codeToBeInserted)
	}

	@Test
	def void testDropTestStepWithDifferenComponentOnLastTestStep() {

		val codeToBeInserted = '''
			Mask: GreetingApplication2
			- Insert "text" into field <Input>'''

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

		var target = getTestStep(testCase, "GreetingApplication", 6)
		var droppedTestStep = createDroppedTestStepContext("GreetingApplication2", "Input", "insertIntoTextField")

		executeTest(droppedTestStep, target, testCase, codeToBeInserted)
	}

	@Test
	def void testDropTestStepWithDifferenComponentOnComponent() {

		val codeToBeInserted = '''
			Mask: GreetingApplication2
			- Insert "text" into field <Input>
			'''

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
		
		var droppedTestStep = createDroppedTestStepContext("GreetingApplication2", "Input", "insertIntoTextField")
		var target = getTestStep(testCase, "GreetingApplication", null)

		executeTest(droppedTestStep, target, testCase, codeToBeInserted)
	}

	@Test
	def void testDropTestStepOnTclModel() {

		val codeToBeInserted = '''
			Mask: GreetingApplication2
			- Insert "text" into field <Input>
			'''

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

		var droppedTestStep = createDroppedTestStepContext("GreetingApplication2", "Input", "insertIntoTextField")
		var target = tclModel

		executeTest(droppedTestStep, target, testCase, codeToBeInserted)
	}
	@Test
	def void testDropTestStepOnTestSpecification() {

		val codeToBeInserted = '''			
			Mask: GreetingApplication2
			- Insert "text" into field <Input>
			'''

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
		'''
		setTclModel(testCase)

		var droppedTestStep = createDroppedTestStepContext("GreetingApplication2", "Input", "insertIntoTextField")
		var target = tclModel.test.steps.last

		executeTest(droppedTestStep, target, testCase, codeToBeInserted)
	}

	def getComponent(String componentName) {
		for (component : amlModel.components) {
			if (component.name == componentName) {
				return component
			}
		}
		throw new Exception("No component found for '" + componentName + "'")
	}

	def getInteractionType(Component component, String interactionTypeName) {
		for (interactionType : component.type.interactionTypes) {
			if (interactionType.name == interactionTypeName) {
				return interactionType
			}
		}
		throw new Exception(
			"No interactiontype found for component '" + component.name + "' and interactionType '" +
				interactionTypeName + "'")
	}

	def createDroppedTestStepContext(String componentName, String interacionType) {
		val component = getComponent(componentName)
		val interactionType = getInteractionType(component, interacionType)

		listener.createDroppedTestStepContext(interactionType, null, component)
	}

	def createDroppedTestStepContext(String componentName, String componentElementName, String interacionType) {
		val component = getComponent(componentName)
		val componentElement = getComponentElement(componentName, componentElementName)
		val interactionType = getInteractionTypeForComponentElement(componentElement, interacionType)

		listener.createDroppedTestStepContext(interactionType, componentElement, component)
	}

	def getInteractionTypeForComponentElement(ComponentElement componentElement, String interactionTypeName) {
		for (interactionType : componentElement.type.interactionTypes) {
			if (interactionType.name == interactionTypeName) {
				return interactionType
			}
		}
		throw new Exception(
			"No element found for interactionTypeName: '" + interactionTypeName + "' in componentElement: '" +
				componentElement.name + "'")
	}

	def executeTest(ComponentTestStepContext droptedTestStepContext, EObject dropTarget, String testCase,
		String insertedCode) {
		val expectedTestCase = testCase.replaceAll("-->INSERT HERE", insertedCode.indent(1))

		listener.updateTestModel(droptedTestStepContext, tclModel.test, dropTarget, newArrayList);
		serializer.serialize(tclModel).assertEquals(expectedTestCase)
	}

	def getComponentElement(String componentName, String elementName) {
		for (component : amlModel.components) {
			if (component.name == componentName) {
				for (element : component.elements) {
					if (element.name == elementName) {
						return element
					}
				}
			}
		}
		throw new Exception("Not element found for component: '" + componentName + "' element: '" + elementName + "'")
	}

	def getTestStep(String testCase, String componentTestStepName, Integer position) {

		for (context : tclModel.test.steps.head.contexts) {
			if ((context as ComponentTestStepContext).component.name == componentTestStepName) {
				if (position == null) {
					return (context as ComponentTestStepContext);
				}
				return (context as ComponentTestStepContext).steps.get(position)
			}
		}
		throw new Exception("component with name '" + componentTestStepName + "' not found")
	}

	protected def String indent(CharSequence input, int level) {
		val indentation = Strings.repeat('\t', level)
		val value = input.toString.replaceAll("(?m)^(?!\r\n)", indentation)
		return value
	}
	def setTclModel(String testCase) {
		tclModel = parserHelper.parseTcl(testCase.replaceAll("-->INSERT HERE", ""), "SwingDemoEins.tcl")
	}

}
