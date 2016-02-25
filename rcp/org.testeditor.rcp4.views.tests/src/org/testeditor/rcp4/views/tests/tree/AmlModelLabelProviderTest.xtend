package org.testeditor.rcp4.views.tests.tree

import javax.inject.Inject
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1
import org.junit.Test
import org.testeditor.aml.AmlFactory
import org.testeditor.aml.TemplateText
import org.testeditor.aml.TemplateVariable
import org.testeditor.rcp4.views.AmlModelLabelProvider

class AmlModelLabelProviderTest extends org.testeditor.aml.dsl.tests.parser.AbstractParserTest {

	@Inject
	AmlModelLabelProvider amlModelLabelProvider

// TODO: get this injected!
//	@Inject
//	AmlFactory factory
	AmlFactory factory = AmlFactory.eINSTANCE

	@Test
	def void labelForModelIsPackage() {
		// given
		val amlModel = factory.createAmlModel
		amlModel.^package = "test"

		// when
		val result = amlModelLabelProvider.getText(amlModel)

		// then
		assertEquals(result, "test")
	}

	@Test
	def void labelForComponentIsName() {
		// given
		val component = factory.createComponent
		component.name = "test"

		// when
		val result = amlModelLabelProvider.getText(component)

		// then
		assertEquals(result, "test")
	}

	@Test
	def void labelForComponentElementIsName() {
		// given
		val componentElement = factory.createComponentElement
		componentElement.name = "test"

		// when
		val result = amlModelLabelProvider.getText(componentElement)

		// then
		assertEquals(result, "test")
	}

	@Test
	def void labelForInteractionTypesIsTheTemplateString() {
		// given
		val interactionType = factory.createInteractionType
		interactionType.template = factory.createTemplate
		interactionType.template.contents += #[
			newTemplateText[value = "Insert Text"],
			newTemplateVariable[name = "text"],
			newTemplateText[value = "into"],
			newTemplateVariable[name = "element"]
		]

		// when
		val result = amlModelLabelProvider.getText(interactionType)

		// then
		assertEquals(result, "Insert Text \"text\" into <element>")
	}

	private def TemplateText newTemplateText(Procedure1<TemplateText> init) {
		val result = factory.createTemplateText
		init.apply(result)
		return result
	}

	private def TemplateVariable newTemplateVariable(Procedure1<TemplateVariable> init) {
		val result = factory.createTemplateVariable
		init.apply(result)
		return result
	}

}
