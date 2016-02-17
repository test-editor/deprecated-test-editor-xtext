package org.testeditor.rcp4.views.tests.tree

import javax.inject.Inject
import org.junit.Test
import org.testeditor.aml.AmlFactory
import org.testeditor.rcp4.views.AmlModelLabelProvider

import static org.hamcrest.CoreMatchers.is
import org.testeditor.aml.TemplateText
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1
import org.testeditor.aml.TemplateVariable

class AmlModelLabelProviderTest extends AbstractParserTest {

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
		assertThat(result, is("test"))
	}

	@Test
	def void labelForComponentIsName() {
		// given
		val component = factory.createComponent
		component.name = "test"

		// when
		val result = amlModelLabelProvider.getText(component)

		// then
		assertThat(result, is("test"))
	}

	@Test
	def void labelForComponentElementIsName() {
		// given
		val componentElement = factory.createComponentElement
		componentElement.name = "test"

		// when
		val result = amlModelLabelProvider.getText(componentElement)

		// then
		assertThat(result, is("test"))
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
		assertThat(result, is("Insert Text \"text\" into <element>"))
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
