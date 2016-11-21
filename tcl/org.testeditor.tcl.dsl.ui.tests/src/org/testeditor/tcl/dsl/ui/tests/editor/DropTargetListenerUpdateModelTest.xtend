package org.testeditor.tcl.dsl.ui.tests.editor

import javax.inject.Inject
import org.junit.Before
import org.testeditor.tcl.dsl.ui.editor.DropTargetXtextEditorListener
import org.junit.Test
import org.testeditor.dsl.common.testing.DslParseHelper
import org.testeditor.tcl.ComponentTestStepContext
import org.eclipse.xtext.serializer.ISerializer
import org.testeditor.tcl.dsl.TclStandaloneSetup
import java.nio.file.Files
import java.nio.file.Paths
import org.testeditor.aml.AmlModel
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.Component
import org.testeditor.tcl.TclModel
import org.eclipse.emf.ecore.EObject
import org.testeditor.dsl.common.testing.AbstractTest

class DropTargetListenerUpdateModelTest extends AbstractTest {

	var AmlModel amlModel = null
	var TclModel tclModel = null

	@Before
	def void parseAmlModel() {
		var byte[] encoded = Files.readAllBytes(
			Paths.get("C:\\development\\runtime-testeditor.product\\SwingDemo\\src\\test\\java\\SwingDemo\\test.aml"))
		amlModel = parserHelper.parseAml(new String(encoded));
		tclModel = parserHelper.parseTcl(testCase, "SwingDemoEins.tcl")

		val tclInjector = (new TclStandaloneSetup).createInjectorAndDoEMFRegistration
		serializer = tclInjector.getInstance(ISerializer)
	}

	@Inject protected extension DslParseHelper parserHelper
	@Inject DropTargetXtextEditorListener listener
	var ISerializer serializer
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
		
			Mask: GreetingApplication2
			- Starte2 application "path"
	'''

	@Test
	def void testDropTestStepOnFirstTestStepContext() {

		var droptedTestStep = createDroppedTestStepContext("GreetingApplication", "test")
		var target = getTestStep("GreetingApplication", null)

		val expectedTestCase = '''
			package SwingDemo
			
			# SwingDemoEins
			
			*
			
				Mask: GreetingApplication
				- Inserted step "path"
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

		executeTest(droptedTestStep, target, expectedTestCase)
		
	}
	
	@Test
	def void testDropTestStepOnFirstTestStep() {

		var dropTarget = getTestStep("GreetingApplication", 0)

		val expectedTestCase = '''
			package SwingDemo
			
			# SwingDemoEins
			
			*
			
				Mask: GreetingApplication
				- Start application "path"
				- Inserted step "path"
				- Stop application
				- Start application "path"
				- Stop application
				- Start application "path"
				- Stop application
				- Wait "miliSeconds" ms
			
				Mask: GreetingApplication2
				- Starte2 application "path"
		'''

		var droppedTestStep = createDroppedTestStepContext("GreetingApplication", "test")

		executeTest(droppedTestStep, dropTarget,  expectedTestCase)
	}
	
	@Test
	def void testDropTestSetpOnThirdTestStep() {

		var target = getTestStep("GreetingApplication", 2)

		val expectedTestCase = '''
			package SwingDemo
			
			# SwingDemoEins
			
			*
			
				Mask: GreetingApplication
				- Start application "path"
				- Stop application
				- Start application "path"
				- Inserted step "path"
				- Stop application
				- Start application "path"
				- Stop application
				- Wait "miliSeconds" ms
			
				Mask: GreetingApplication2
				- Starte2 application "path"
		'''

		var droptedTestStep = createDroppedTestStepContext("GreetingApplication", "test")

		executeTest(droptedTestStep, target,  expectedTestCase)
	}

	@Test
	def void testTestStepDropOnLastTestStep() {

		var target = getTestStep("GreetingApplication", 6)

		val expectedTestCase = '''
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
				- Inserted step "path"
			
				Mask: GreetingApplication2
				- Starte2 application "path"
		'''
		
		var droptedTestStep = createDroppedTestStepContext("GreetingApplication", "test")


		executeTest(droptedTestStep, target, expectedTestCase)
	}
	@Test
	def void testDropOtherTestComponentOnFirstStep() {

		var target = getTestStep("GreetingApplication", 0)

		val expectedTestCase = '''
			package SwingDemo
			
			# SwingDemoEins
			
			*
			
				Mask: GreetingApplication
				- Start application "path"
			
				Mask: GreetingApplication2
				- Insert "text" into field <Input>
			
				Mask: GreetingApplication
				- Stop application
				- Start application "path"
				- Stop application
				- Start application "path"
				- Stop application
				- Wait "miliSeconds" ms
			
				Mask: GreetingApplication2
				- Starte2 application "path"
		'''

		val componentElement = getComponentElement("GreetingApplication2", "Input")
		
		var dropedTestStep = createDropedTestStep("GreetingApplication2","Input", "insertIntoTextField")


		executeTest(dropedTestStep, target, expectedTestCase)
	}

	@Test
	def void testDropOtherTestComponentOnLastStep() {

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
	val insertHere = '''
	«""»
				Mask: GreetingApplication2
				- Insert "text" into field <Input>'''
				
		val expectedTestCase = testCase.replaceAll("-->INSERT HERE",  insertHere)

		var target = getTestStep("GreetingApplication", 6)

		var dropepTestStep = createDropedTestStep("GreetingApplication2", "Input","insertIntoTextField")

		executeTest(dropepTestStep, target, expectedTestCase)
	}

	@Test
	def void testDropOtherTestComponentOnTestComponent() {

		val expectedTestCase = '''
			package SwingDemo
			
			# SwingDemoEins
			
			*
			
				Mask: GreetingApplication2
				- Insert "text" into field <Input>
			
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

		var dropedTestStep = createDropedTestStep("GreetingApplication2", "Input","insertIntoTextField")

		var target = getTestStep("GreetingApplication", null)

		executeTest(dropedTestStep, target, expectedTestCase)
	}
	@Test
	def void testDropTestStepOnTclModel() {

		val expectedTestCase = '''
			package SwingDemo
			
			# SwingDemoEins
			
			*
			
				Mask: GreetingApplication2
				- Insert "text" into field <Input>
			
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

		var dropedTestStep = createDropedTestStep("GreetingApplication2", "Input","insertIntoTextField")
		var target = tclModel

		executeTest(dropedTestStep, target,  expectedTestCase)
	}
	def getComponent(String componentName) {
		for(component : amlModel.components){
			if(component.name == componentName){
				return component
			}
		}
		throw new Exception("No component found for '" + componentName + "'")
	}
	def getInteractionType(Component component, String interactionTypeName) {
				for(interactionType : component.type.interactionTypes) {
					if(interactionType.name == interactionTypeName){
						return interactionType
					}
				}
		throw new Exception("No interactiontype found for component '" + component.name + "' and interactionType '" + interactionTypeName + "'")
	}
	def createDroppedTestStepContext(String componentName, String interacionType) {
		val component = getComponent(componentName)
		val interactionType = getInteractionType(component, interacionType)
		
		listener.createDroppedTestStepContext(interactionType, null, component)
	}
	def createDropedTestStep(String componentName, String componentElementName, String interacionType) {
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
	def executeTest(ComponentTestStepContext droptedTestStepContext, EObject dropTarget, String expectedTestCase) {
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
	def getTestStep(String componentTestStepName, Integer position) {
		for(context : tclModel.test.steps.head.contexts){
			if((context as ComponentTestStepContext).component.name == componentTestStepName){
				if(position == null){
					return (context as ComponentTestStepContext);
				}
				return (context as ComponentTestStepContext).steps.get(position)
			}
		}
		throw new Exception("component with name '" + componentTestStepName + "' not found")
	}
}
