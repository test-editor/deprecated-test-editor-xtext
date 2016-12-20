package org.testeditor.tcl.dsl.ui.tests.editor

import com.google.common.base.Strings
import com.google.common.io.ByteStreams
import com.google.inject.Module
import java.nio.charset.StandardCharsets
import java.util.List
import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.serializer.ISerializer
import org.junit.Before
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.dsl.AmlRuntimeModule
import org.testeditor.dsl.common.testing.AbstractTest
import org.testeditor.dsl.common.testing.DslParseHelper
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.TclRuntimeModule
import org.testeditor.tcl.dsl.ui.editor.DropUtils

class AbstractTclModelDragAndDropUpdaterTest extends AbstractTest {

	@Inject DropUtils dropUtils

	@Inject protected extension DslParseHelper parserHelper
	@Inject protected ISerializer serializer
	var AmlModel amlModel

	override protected collectModules(List<Module> modules) {
		super.collectModules(modules)
		modules += new AmlRuntimeModule
		modules += new TclRuntimeModule
	}

	@Before
	def void parseAmlModel() {
		var encoded = ByteStreams.toByteArray(class.classLoader.getResourceAsStream("test.aml"))
		amlModel = parserHelper.parseAml(new String(encoded, StandardCharsets.UTF_8))
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
		val interactionType = getInteractionType(componentElement, interacionType)

		dropUtils.createDroppedTestStepContext(component, componentElement, interactionType)
	}

	def protected getComponent(String componentName) {
		return amlModel.components.findFirst[name == componentName].assertNotNull
	}

	def protected getInteractionType(Component component, String interactionTypeName) {
		return component.type.interactionTypes.findFirst[name == interactionTypeName].assertNotNull
	}

	def protected getInteractionType(ComponentElement componentElement, String interactionTypeName) {
		return componentElement.type.interactionTypes.findFirst[name == interactionTypeName].assertNotNull
	}

	def protected ComponentElement getComponentElement(String componentName, String elementName) {
		return amlModel.components.findFirst[name == componentName]?.elements?.findFirst[name == elementName].
			assertNotNull
	}

	def protected ComponentTestStepContext getTestStepContext(TclModel tclModel, String componentTestStepName) {
		val contexts = tclModel.test.steps.head.contexts
		return contexts.filter(ComponentTestStepContext).findFirst[component.name == componentTestStepName].assertNotNull		
	}

	def protected EObject getTestStep(TclModel tclModel, String componentTestStepName, Integer position) {
		val context = getTestStepContext(tclModel, componentTestStepName)
		return context.steps.get(position)
	}

	def protected String indent(CharSequence input, int level) {
		val indentation = Strings.repeat('\t', level)
		val value = input.toString.replaceAll('(?m)^(?!\r?\n)', indentation)
		return value
	}

}
