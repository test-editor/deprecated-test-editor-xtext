/*******************************************************************************
 * Copyright (c) 2012 - 2015 Signal Iduna Corporation and others.
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
import org.testeditor.aml.dsl.validation.AmlValidator
import org.testeditor.aml.Component

import static org.testeditor.aml.dsl.Messages.*
import static org.testeditor.aml.dsl.validation.AmlValidator.*
import static org.testeditor.aml.AmlPackage.Literals.*

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