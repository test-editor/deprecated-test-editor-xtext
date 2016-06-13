/*******************************************************************************
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 *******************************************************************************/
package org.testeditor.aml.dsl.tests.parser.validation

import javax.inject.Inject
import org.junit.Test
import org.testeditor.aml.dsl.tests.AmlModelGenerator
import org.testeditor.aml.dsl.validation.AmlValidator
import org.testeditor.dsl.common.testing.DummyFixture
import org.testeditor.dsl.common.testing.DummyLocatorStrategy

import static org.testeditor.aml.AmlPackage.Literals.*
import static org.testeditor.aml.dsl.Messages.*
import static org.testeditor.aml.dsl.validation.AmlValidator.*

/**
 * Tests for {@link AmlValidator}.
 */
class ValidationTest extends AbstractValidationTest {

	@Inject extension AmlModelGenerator

	@Test
	def void validateComponentType() {
		// Given
		val component = component("MyComponent").addToResourceSet("aml")

		// Expect
		component.assertError(COMPONENT, COMPONENT__TYPE__MISSING, Validation_Component_Type_Missing)
	}

	@Test
	def void regExValueSpace() {
		// Given
		val valueSpace = regExValueSpace("${").addToResourceSet("aml")

		// Expect
		valueSpace.assertError(
			REG_EX_VALUE_SPACE,
			REG_EX_VALUE_SPACE__EXPRESSION__INVALID,
			"The given expression is not a valid regular expression in Java:",
			"java.util.regex.PatternSyntaxException: Illegal repetition near index 0",
			"${",
			"^"
		)
	}

	@Test
	def void testUsageOfDefaultLocatorStrategy() {
		// given
		val amlModel = amlModel.withTypeImport(resourceSet, "org.testeditor.dsl.common.testing.DummyLocatorStrategy").
			withTypeImport(resourceSet, "org.testeditor.dsl.common.testing.DummyFixture") => [
			interactionTypes += interactionType("click") => [
				label = "Clickon"
				defaultMethod = methodReference(resourceSet, DummyFixture, "clickOn", "element").withLocatorStrategy
				template = template("click", "on").withParameter(defaultMethod.parameters.head)
				locatorStrategy = locatorStrategy(resourceSet, DummyLocatorStrategy, "SINGLE")
			]
		]
		amlModel.addToResourceSet("aml")
		val interactionType = amlModel.interactionTypes.head

		// expect
		amlModel.assertNoErrors
		interactionType.locatorStrategy.qualifiedName.assertEquals(
			"org.testeditor.dsl.common.testing.DummyLocatorStrategy.SINGLE")
	}

	@Test
	def void testUsageOfElementOverDefaultLocatorStrategy() {
		// given
		val amlModel = amlModel.withTypeImport(resourceSet, "org.testeditor.dsl.common.testing.DummyLocatorStrategy").
			withTypeImport(resourceSet, "org.testeditor.dsl.common.testing.DummyFixture") => [
			val clickInteractionType = interactionType("click") => [
				label = "Clickon"
				defaultMethod = methodReference(resourceSet, DummyFixture, "clickOn", "element").withLocatorStrategy
				template = template("click", "on").withParameter(defaultMethod.parameters.head)
				locatorStrategy = locatorStrategy(resourceSet, DummyLocatorStrategy, "SINGLE")
			]
			interactionTypes += clickInteractionType
			val buttonType = componentElementType("Button") => [
				interactionTypes += clickInteractionType
			]
			componentElementTypes += buttonType
			val dialogType = componentType("Dialog")
			componentTypes += dialogType
			components += component("NewDialog") => [
				type = dialogType
				elements += componentElement("NewButton") => [
					type = buttonType
					locator = "ok"
					locatorStrategy = locatorStrategy(resourceSet, DummyLocatorStrategy, "ID")
				]
			]
		]
		amlModel.addToResourceSet("aml")
		val elementNewButton = amlModel.components.head.elements.head

		// expect
		amlModel.assertNoErrors
		elementNewButton.locatorStrategy.qualifiedName.assertEquals(
			"org.testeditor.dsl.common.testing.DummyLocatorStrategy.ID")
	}

	@Test
	def void testMissingLocatorStrategy() {
		// given
		val amlModel = amlModel.withTypeImport(resourceSet, "org.testeditor.dsl.common.testing.DummyLocatorStrategy").
			withTypeImport(resourceSet, "org.testeditor.dsl.common.testing.DummyFixture") => [
			val clickInteractionType = interactionType("click") => [
				label = "Clickon"
				defaultMethod = methodReference(resourceSet, DummyFixture, "clickOn", "element").withLocatorStrategy
				template = template("click", "on").withParameter(defaultMethod.parameters.head)
			]
			interactionTypes += clickInteractionType
			val buttonType = componentElementType("Button") => [
				interactionTypes += clickInteractionType
			]
			componentElementTypes += buttonType
			val dialogType = componentType("Dialog")
			componentTypes += dialogType
			components += component("NewDialog") => [
				type = dialogType
				elements += componentElement("NewButton") => [
					type = buttonType
					locator = "ok"
				]
			]
		]
		amlModel.addToResourceSet("aml")

		// expect
		amlModel.assertError(
			COMPONENT_ELEMENT,
			COMPONENT_ELEMENT__LOCATOR_STRATEGY__MISSING
		)
	}

	@Test
	def void testMissingLocatorStrategyNotNeeded() {
		// given
		val amlModel = amlModel.withTypeImport(resourceSet, "org.testeditor.dsl.common.testing.DummyLocatorStrategy").
			withTypeImport(resourceSet, "org.testeditor.dsl.common.testing.DummyFixture") => [
			val getValueInteractionType = interactionType("getValue") => [
				label = "StartApplication"
				defaultMethod = methodReference(resourceSet, DummyFixture, "getValue", "element")
				template = template("get", "value", "from").withParameter(defaultMethod.parameters.head)
			]
			interactionTypes += getValueInteractionType
			val buttonType = componentElementType("Button") => [
				interactionTypes += getValueInteractionType
			]
			componentElementTypes += buttonType
			val dialogType = componentType("Dialog")
			componentTypes += dialogType
			components += component("NewDialog") => [
				type = dialogType
				elements += componentElement("NewButton") => [
					type = buttonType
					locator = "ok"
				]
			]
		]
		amlModel.addToResourceSet("aml")

		// expect
		amlModel.assertNoErrors
	}
}
