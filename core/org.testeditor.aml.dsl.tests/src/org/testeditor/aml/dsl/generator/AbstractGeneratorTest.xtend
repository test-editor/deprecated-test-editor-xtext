package org.testeditor.aml.dsl.generator

import org.testeditor.aml.dsl.tests.AbstractTest
import org.testeditor.aml.model.ModelFactory

abstract class AbstractGeneratorTest extends AbstractTest {

	protected static val factory = ModelFactory.eINSTANCE

	def createAmlModel() {
		return factory.createAmlModel
	}
	
	def createInteractionType(String name, String label) {
		return factory.createInteractionType => [
			it.name = name
			it.label = label
		]
	}

	def createTemplateText(String value) {
		return factory.createTemplateText => [
			it.value = value
		]
	}

	def createTemplateVariable(String name) {
		return factory.createTemplateVariable => [
			it.name = name
		]
	}

}