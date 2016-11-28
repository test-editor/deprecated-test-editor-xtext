package org.testeditor.tcl.dsl.ui.tests.editor

import com.google.common.base.Strings
import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.serializer.ISerializer
import org.junit.Before
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.dsl.common.testing.AbstractTest
import org.testeditor.dsl.common.testing.DslParseHelper
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.TclStandaloneSetup
import com.google.common.io.ByteStreams
import org.testeditor.tcl.dsl.ui.editor.DropUtils
import org.testeditor.tcl.dsl.ui.editor.UpdateTestModelByDropTarget

class AbstractTestModelByDropTargetTest extends AbstractTest {

	@Inject UpdateTestModelByDropTarget updateTestModelByDropTarget
	@Inject DropUtils dropUtils

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

	def protected getComponent(String componentName) {
		for (component : amlModel.components) {
			if (component.name == componentName) {
				return component
			}
		}
		throw new Exception("No component found for '" + componentName + "'")
	}

	def protected getInteractionType(Component component, String interactionTypeName) {
		for (interactionType : component.type.interactionTypes) {
			if (interactionType.name == interactionTypeName) {
				return interactionType
			}
		}
		throw new Exception(
			"No interactiontype found for component '" + component.name + "' and interactionType '" +
				interactionTypeName + "'")
	}

	def protected createDroppedTestStepContext(String componentName, String interacionType) {
		val component = getComponent(componentName)
		val interactionType = getInteractionType(component, interacionType)

		dropUtils.createDroppedTestStepContext(component, null, interactionType)
	}

	def protected createDroppedTestStepContext(String componentName, String componentElementName,
		String interacionType) {
		val component = getComponent(componentName)
		val componentElement = getComponentElement(componentName, componentElementName)
		val interactionType = getInteractionTypeForComponentElement(componentElement, interacionType)

		dropUtils.createDroppedTestStepContext(component, componentElement, interactionType)
	}

	def protected getInteractionTypeForComponentElement(ComponentElement componentElement, String interactionTypeName) {
		for (interactionType : componentElement.type.interactionTypes) {
			if (interactionType.name == interactionTypeName) {
				return interactionType
			}
		}
		throw new Exception(
			"No element found for interactionTypeName: '" + interactionTypeName + "' in componentElement: '" +
				componentElement.name + "'")
	}

	def protected executeTest(ComponentTestStepContext newTestStepContext, EObject dropTarget, String testCase,
		String insertedCode) {
		val expectedTestCase = testCase.replaceAll("-->INSERT HERE", insertedCode.indent(1)).replace("\r\n", "\n")

		updateTestModelByDropTarget.updateTestModel(tclModel.test, dropTarget, newTestStepContext, newArrayList);
		val actualTestCase = serializer.serialize(tclModel).replace("\r\n", "\n")
		actualTestCase.assertEquals(expectedTestCase)
	}

	def protected getComponentElement(String componentName, String elementName) {
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

	def protected getTestStep(String testCase, String componentTestStepName, Integer position) {

		for (context : tclModel.test.steps.head.contexts) {
			if ((context as ComponentTestStepContext).component.name == componentTestStepName) {
				if (position === null) {
					return (context as ComponentTestStepContext);
				}
				return (context as ComponentTestStepContext).steps.get(position)
			}
		}
		throw new Exception("component with name '" + componentTestStepName + "' not found")
	}

	def protected String indent(CharSequence input, int level) {
		val indentation = Strings.repeat('\t', level)
		val value = input.toString.replaceAll("(?m)^(?!\r?\n)", indentation)
		return value
	}

	def protected setTclModel(String testCase) {
		tclModel = parserHelper.parseTcl(testCase.replaceAll("-->INSERT HERE", ""), "SwingDemoEins.tcl")
	}

	def protected TclModel getTclModel() {
		return tclModel
	}

}
