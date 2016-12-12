package org.testeditor.aml.dsl.scoping

import org.junit.Before
import org.junit.Test
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.ComponentElementType
import org.testeditor.aml.ComponentType
import org.testeditor.aml.InteractionType
import org.testeditor.aml.ValueSpaceAssignment
import org.testeditor.aml.ValueSpaceAssignmentContainer

/**
 * Tests parsing and scoping for {@link ValueSpaceAssignment}.
 */
class ValueSpaceAssignmentScopingTest extends AbstractScopingTest {

	@Before
	def void parseValueSpaceAndInteraction() {
		val aml = '''
			package com.example
			
			value-space Integer = "\\d+"
			
			interaction type EnterValue {
				template = "Enter" ${value} "into" ${element}
			}
		'''
		parseAml(aml).assertNoErrors
	}

	@Test
	def void interactionType() {
		// given
		val source = '''
			interaction type EnterIntegerValue {
				template = "Enter" ${integerValue} "into" ${element}
				restrict integerValue to Integer
			}
		'''

		// when
		val element = source.parseAmlWithStdPackage(InteractionType)

		// then
		element.assertHasRestrictionOnVariable('integerValue')
	}

	@Test
	def void componentType() {
		// given
		val source = '''
			component type Dialog {
				interactions = EnterValue
				restrict EnterValue.value to Integer
			}
		'''

		// when
		val element = source.parseAmlWithStdPackage(ComponentType)

		// then
		element.assertHasRestrictionOnVariable('value')
	}

	@Test
	def void component() {
		// given
		val source = '''
			component type Dialog {
				interactions = EnterValue
			}
			
			component MyDialog is Dialog {
				restrict EnterValue.value to Integer
			}
		'''

		// when
		val element = source.parseAmlWithStdPackage(Component)

		// then
		element.assertHasRestrictionOnVariable('value')
	}

	@Test
	def void componentElementType() {
		// given
		val source = '''
			element type Text {
				interactions = EnterValue
				restrict EnterValue.value to Integer
			}
		'''

		// when
		val element = source.parseAmlWithStdPackage(ComponentElementType)

		// then
		element.assertHasRestrictionOnVariable('value')
	}

	@Test
	def void componentElement() {
		// given
		val source = '''
			component type Dialog
			element type Text {
				interactions = EnterValue
			}
			
			component MyDialog is Dialog {
				element MyInput is Text {
					restrict EnterValue.value to Integer
				}
			}
		'''

		// when
		val element = source.parseAmlWithStdPackage(ComponentElement)

		// then
		element.assertHasRestrictionOnVariable('value')
	}

	private def void assertHasRestrictionOnVariable(ValueSpaceAssignmentContainer element, String name) {
		element.assertNoErrors
		element.valueSpaceAssignments.assertSingleElement => [
			variable.name.assertEquals(name)
		]
	}

}
