package org.testeditor.aml.dsl.tests.parser.validation

import org.junit.Test
import org.testeditor.aml.dsl.validation.AmlValidator
import org.testeditor.aml.model.Component

import static org.testeditor.aml.dsl.Messages.*
import static org.testeditor.aml.dsl.validation.AmlValidator.*
import static org.testeditor.aml.model.ModelPackage.Literals.*

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
	
}