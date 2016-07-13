package org.testeditor.aml.dsl.tests.common

import javax.inject.Inject
import org.eclipse.emf.ecore.resource.ResourceSet
import org.testeditor.aml.AmlModel
import org.testeditor.aml.dsl.tests.AmlModelGenerator
import org.testeditor.dsl.common.testing.DummyFixture

class AmlTestModels {
	@Inject extension AmlModelGenerator
	
	public val COMPONENT_NAME = "Dummy"
	public val COMPONENT_TYPE_NAME = "DummyCT"

	/** build an aml model with
	 *		one interaction type "start" calling DummyFixture.startApplication,
	 * 		one component type COMPONENT_TYPE_NAME with the "start" interaction,
	 * 		one component COMPONENT_NAME which is of this component type.
	 */
	def AmlModel dummyComponent(ResourceSet resourceSet) {
		return amlModel => [
			withNamespaceImport(DummyFixture.package.name + '.*')
			val startInteraction = interactionType("start") => [
				defaultMethod = methodReference(resourceSet, DummyFixture, "startApplication", "appname")
				template = template("start").withParameter(defaultMethod.parameters.head)
			]
			interactionTypes += startInteraction

			val dummyComponentType = componentType(COMPONENT_TYPE_NAME) => [
				interactionTypes += startInteraction
			]
			componentTypes += dummyComponentType

			components += component(COMPONENT_NAME) => [type = dummyComponentType]
		]
	}

}
