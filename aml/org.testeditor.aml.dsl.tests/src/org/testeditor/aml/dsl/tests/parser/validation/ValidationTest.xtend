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

import org.junit.Test
import org.testeditor.aml.Component
import org.testeditor.aml.ValueSpace
import org.testeditor.aml.dsl.validation.AmlValidator

import static org.testeditor.aml.AmlPackage.Literals.*
import static org.testeditor.aml.dsl.Messages.*
import static org.testeditor.aml.dsl.validation.AmlValidator.*

/**
 * Tests for {@link AmlValidator}.
 */
class ValidationTest extends AbstractValidationTest {

	@Test
	def void validateComponentType() {
		// Given
		val input = '''
			component MyComponent
		'''

		// When
		val component = input.parse(Component)

		// Then
		component.assertError(COMPONENT, COMPONENT__TYPE__MISSING, Validation_Component_Type_Missing)
	}

	@Test
	def void regExValueSpace() {
		// Given
		val input = '''
			value-space invalid="${"
		'''

		// Expect
		input.parse(ValueSpace).assertError(
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
		val input = '''
			package org.test

			import org.testeditor.dsl.common.testing.DummyLocatorStrategy
			import org.testeditor.dsl.common.testing.DummyFixture

			interaction type click {
				label = "Click on"
				template = "click on" ${element}
				method = DummyFixture.clickOn(element, locatorStrategy)
				locatorStrategy = SINGLE
			}
		'''

		// when
		val amlModel = input.parse
		amlModel.assertNoErrors
		val interactionType = amlModel.interactionTypes.head

		// then
		interactionType.locatorStrategy.qualifiedName.assertEquals(
			"org.testeditor.dsl.common.testing.DummyLocatorStrategy.SINGLE")
	}

	@Test
	def void testUsageOfElementOverDefaultLocatorStrategy() {
		// given
		val input = '''
			package org.test

			import org.testeditor.dsl.common.testing.DummyLocatorStrategy
			import org.testeditor.dsl.common.testing.DummyFixture

			interaction type click {
				label = "Click on"
				template = "click on" ${element}
				method = DummyFixture.clickOn(element, locatorStrategy)
				locatorStrategy = SINGLE
			}

			element type Button {
				interactions = click
			}

			component type Dialog { }

			component NewDialog is Dialog {
				element NewButton is Button {
					locator = "ok"
					locatorStrategy = ID
				}
			}
		'''

		// when
		val amlModel = input.parse
		amlModel.assertNoErrors
		val elementNewButton = amlModel.components.head.elements.head

		// then
		elementNewButton.locatorStrategy.qualifiedName.assertEquals(
			"org.testeditor.dsl.common.testing.DummyLocatorStrategy.ID")
	}

	@Test
	def void testMissingLocatorStrategy() {
		// given
		val input = '''
			package org.test

			import org.testeditor.dsl.common.testing.DummyLocatorStrategy
			import org.testeditor.dsl.common.testing.DummyFixture

			interaction type click {
				label = "Click on"
				template = "click on" ${element}
				method = DummyFixture.clickOn(element, locatorStrategy)
			}

			element type Button {
				interactions = click
			}

			component type Dialog { }

			component NewDialog is Dialog {
				element NewButton is Button {
					locator = "ok"
				}
			}
		'''

		// when
		val amlModel = input.parse

		// then
		amlModel.assertError(
			COMPONENT_ELEMENT,
			COMPONENT_ELEMENT__LOCATOR_STRATEGY__MISSING
		)
	}

	@Test
	def void testForDuplicateInteractionNames() {
		// given
		val input = '''
			package com.example
			
			interaction type abc { }
			interaction type efg { }
			interaction type abc { }
		'''
		val startOfFirstAbc = input.indexOf("interaction type abc")
		val startOfSecondAbc = input.indexOf("interaction type abc", startOfFirstAbc + 1)
		val lengthOfinteractionType = 24

		// when
		val amlModel = input.parse

		// then
		amlModel.assertError(AML_MODEL, INTERACTION_NAME_DUPLICATION, startOfFirstAbc, lengthOfinteractionType,
			"has name ('abc')")
		amlModel.assertError(AML_MODEL, INTERACTION_NAME_DUPLICATION, startOfSecondAbc, lengthOfinteractionType,
			"has name ('abc')")

	}

	@Test
	def void testUniqueInteractionNames() {
		// given
		val input = '''
			package com.example
			
			interaction type efg { }
			interaction type abc { }
		'''
		// when
		val amlModel = input.parse

		// then
		amlModel.assertNoErrors
	}

}
