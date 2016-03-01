package org.testeditor.rcp4.views.teststepselector

import javax.inject.Inject
import org.junit.Test
import org.testeditor.aml.AmlFactory
import org.testeditor.aml.TemplateContent
import org.testeditor.aml.dsl.tests.parser.AbstractParserTest
import org.testeditor.rcp4.views.teststepselector.AmlModelLabelProvider

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
		val amlModel = factory.createAmlModel => [^package = "test"]

		// when
		val result = amlModelLabelProvider.getText(amlModel)

		// then
		assertEquals(result, "test")
	}

	@Test
	def void labelForComponentIsName() {
		// given
		val component = factory.createComponent => [name = "test"]

		// when
		val result = amlModelLabelProvider.getText(component)

		// then
		assertEquals(result, "test")
	}

	@Test
	def void labelForComponentElementIsName() {
		// given
		val componentElement = factory.createComponentElement => [name = "test"]

		// when
		val result = amlModelLabelProvider.getText(componentElement)

		// then
		assertEquals(result, "test")
	}

	@Test
	def void labelForInteractionTypesIsTheTemplateString() {
		// given
		val interactionType = factory.createInteractionType => [
			template = factory.createTemplate => [
				contents += #[
					factory.createTemplateText => [value = "Insert Text"],
					factory.createTemplateVariable => [name = "text"],
					factory.createTemplateText => [value = "into"],
					factory.createTemplateVariable => [name = "element"]
				].map[it as TemplateContent]
			]
		]

		// when
		val result = amlModelLabelProvider.getText(interactionType)

		// then
		assertEquals(result, 'Insert Text "text" into <element>')
	}

}
