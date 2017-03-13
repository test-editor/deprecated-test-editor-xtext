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
	 *		one interaction type "start" calling DummyiFxture.startApplication,
	 *		one interaction type "getValue" calling DummyFixture.getValue,
	 *		one interaction type "wait" calling DummyFixture.waitSeconds,
	 *		one interaction type "getMap" calling DummyFixture.getMap
	 * 		one component type COMPONENT_TYPE_NAME with the "start", "wait", "getMap" and "getValue" interaction,
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
			
			val interactionTypeForGetSome = interactionType("getValue") => [
				defaultMethod = methodReference(resourceSet, DummyFixture, "getValue", "element")
				template = template("getValue").withParameter(defaultMethod.parameters.head)
			]
			interactionTypes += interactionTypeForGetSome
			
			val interactionTypeForWait = interactionType("wait") => [
				defaultMethod = methodReference(resourceSet, DummyFixture, "waitSeconds", "secs")
				template = template("wait").withParameter(defaultMethod.parameters.head)
			]					
			interactionTypes += interactionTypeForWait

			val interactionTypeIntoAndWait = interactionType("typeIntoAndWait") => [
				defaultMethod = methodReference(resourceSet, DummyFixture, "typeIntoAndWait", "element", "locatorStrategy", "value", "seconds")
				val secondsParameter = defaultMethod.parameters.findFirst[name == 'seconds']
				val valueParameter = defaultMethod.parameters.findFirst[name == 'value']
				template = template("type").withParameter(valueParameter).withText("into").withParameter("element").
					withText("and wait").withParameter(secondsParameter)
			]					
			interactionTypes += interactionTypeIntoAndWait

			val interactionTypeForGetMap = interactionType("getMap") => [
				defaultMethod = methodReference(resourceSet, DummyFixture, "getMap", "element")
				template = template("getMap").withParameter(defaultMethod.parameters.head)
			]					
			interactionTypes += interactionTypeForGetMap

			val dummyComponentType = componentType(COMPONENT_TYPE_NAME) => [
				interactionTypes += startInteraction
				interactionTypes += interactionTypeForGetSome
				interactionTypes += interactionTypeForWait
				interactionTypes += interactionTypeForGetMap
				interactionTypes += interactionTypeIntoAndWait
			]
			componentTypes += dummyComponentType
			
			val dummyElementType = componentElementType("dummyElementType")
			componentElementTypes += dummyElementType

			components += component(COMPONENT_NAME) => [
				type = dummyComponentType
				elements += componentElement("dummyElement") => [
					type = dummyElementType
					locator = "dummyLocator"
				]
			]
		]
	}

}
