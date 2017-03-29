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

import java.text.MessageFormat
import com.google.inject.Inject
import org.junit.Test
import org.testeditor.aml.dsl.tests.AmlModelGenerator
import org.testeditor.aml.dsl.tests.parser.AbstractParserTest
import org.testeditor.aml.dsl.validation.AmlValidator
import org.testeditor.dsl.common.testing.DummyFixture
import org.testeditor.dsl.common.testing.DummyLocatorStrategy

import static org.testeditor.aml.AmlPackage.Literals.*
import static org.testeditor.aml.dsl.Messages.*
import static org.testeditor.aml.dsl.validation.AmlValidator.*

/**
 * Tests for {@link AmlValidator}.
 */
class ValidationTest extends AbstractParserTest {

	@Inject extension AmlModelGenerator

	@Test
	def void validateComponentType() {
		// Given
		val component = component("MyComponent").addToResourceSet("Dummy.aml")

		// Expect
		component.assertError(COMPONENT, COMPONENT__TYPE__MISSING, Validation_Component_Type_Missing)
	}

	@Test
	def void regExValueSpace() {
		// Given
		val valueSpace = regExValueSpace("${").addToResourceSet("Dummy.aml")

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
		amlModel.addToResourceSet("Dummy.aml")
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
		amlModel.addToResourceSet("Dummy.aml")
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
		amlModel.addToResourceSet("Dummy.aml")

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
		amlModel.addToResourceSet("Dummy.aml")

		// expect
		amlModel.assertNoErrors
	}

	@Test
	def void testForDuplicateInteractionNames() {
		// given (as source to check error marker location)
		val input = '''
			package com.example
			
			interaction type abc { }
			interaction type efg { }
			interaction type abc { }
		'''
		val startOfFirstAbc = input.indexOf("interaction type abc")
		val startOfSecondAbc = input.indexOf("interaction type abc", startOfFirstAbc + 1)
		val lengthOfinteractionType = "interaction type abc { }".length

		// when
		val amlModel = input.parseAml

		// then
		val message = MessageFormat.format(Validation_InteractionType_Name_Dublicate, "abc")

		amlModel.assertError(INTERACTION_TYPE, INTERACTION_NAME_DUPLICATION, startOfFirstAbc, lengthOfinteractionType,
			message)
		amlModel.assertError(INTERACTION_TYPE, INTERACTION_NAME_DUPLICATION, startOfSecondAbc, lengthOfinteractionType,
			message)

	}

	@Test
	def void testForDuplicateTempalteCode() {
		// given (as source to check error marker location)
		val interactionCodeLength = '''
		interaction type efg { 
			template = "templateB"
		}'''.length
		val componentCodeLength = '''
		component type doublicateTemplateCode {
			interactions = abc, efg, hij
		}'''.length
		val input = '''
			package com.example
			
			interaction type abc { 
				template = "templateA"
			}
			interaction type efg { 
				template = "templateB"
			}
			interaction type hij { 
				template = "templateA"
			}
			component type noProblem {
				interactions = abc, efg
			}
			component type doublicateTemplateCode {
				interactions = abc, efg, hij
			}
		'''
		val startOfInteraction1 = input.indexOf("interaction type abc")
		val startOfInteraction3 = input.indexOf("interaction type hij")
		val startOfComponent = input.indexOf("component type doublicateTemplateCode")
		val lengthOfinteractionType = interactionCodeLength
		val lengthOfComponentType = componentCodeLength

		// when
		val amlModel = input.parseAml

		// then
		val message = Validation_TemplateCode_NotUnique

		amlModel.assertError(INTERACTION_TYPE, TEMPLATE_CODE_NOT_UNIQUE, startOfInteraction3, lengthOfinteractionType,
			message)
		amlModel.assertError(INTERACTION_TYPE, TEMPLATE_CODE_NOT_UNIQUE, startOfInteraction1, lengthOfinteractionType,
			message)
		amlModel.assertError(COMPONENT_TYPE, TEMPLATE_CODE_NOT_UNIQUE, startOfComponent, lengthOfComponentType, message)

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
		val amlModel = input.parseAml

		// then
		amlModel.assertNoErrors
	}
	
	@Test
	def void testInvalidTemplates() {
		// given		
		val invalidTemplates = #[
			'"invalid because containing a comma, which is not valid"',
			'"invalid because numbers like 123 are not allowed"',
			'"invalid because" ${param} "exclamation mark ? is not allowed in the middle"'
		]
        // when, then
		invalidTemplates.forEach[assertInvalidTemplate("Illegal character")]
	}

	@Test
	def void testInvalidTemplatesInLastText() {
		// given
		val invalidTemplates = #[
			'"invalid because . is allowed at the end only"',
			'"invalid because last element is not valid punctuation at end" ${param} "="'
		]
        // when, then
		invalidTemplates.forEach[assertInvalidTemplate("last element")]
	}

	@Test
	def void testInvalidEmptyTemplates() {
		// given
		val invalidTemplates = #[
			'""',
			'"       "',
			'"invalid because empty part follows" ${param} "        "'
		]
        // when, then
		invalidTemplates.forEach[assertInvalidTemplate("empty")]
	}

	def private void assertInvalidTemplate(String template, String errorMsgPart) {
		// given
		val input = '''
			package com.example
			
			interaction type abc {
				template = «template»
			}
		'''
		// when
		val amlModel = input.parseAml

		// then
		amlModel.assertError(TEMPLATE, INVALID_CHAR_IN_TEMPLATE, errorMsgPart)
	}

	@Test
	def void testValidTemplate() {
		// given
		val input = '''
			package com.example
			
			interaction type abc {
				template = "valid  _1_  ^because  $ot$her " ${param} " only valid02 elements u see?"
			}
		'''
		// when
		val amlModel = input.parseAml

		// then
		amlModel.assertNoErrors
	}

}
