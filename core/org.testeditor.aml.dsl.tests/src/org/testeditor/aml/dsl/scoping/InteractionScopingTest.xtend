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
package org.testeditor.aml.dsl.scoping

import javax.inject.Inject
import org.junit.Test
import org.testeditor.aml.dsl.tests.parser.AbstractParserTest
import org.testeditor.aml.model.ModelUtil

/**
 * Tests scoping for interactions / template variables.
 */
class InteractionScopingTest extends AbstractParserTest {

	@Inject extension ModelUtil

	@Test
	def void testImportedInteractionScope() {
		// Given
		val file1 = '''
			package com.example
			
			interaction type Resize {
				template = "Resize" ${element} "to size" ${size}
			}
			
			component type Dialog {
				interactions = Resize
			}
		'''
		val file2 = '''
			package org.testeditor
			
			import com.example.*
			
			component MyDialog is Dialog {
				Resize.size restrict to ResizePercent
			}
			
			value-space ResizePercent = 10 .. 100
		'''

		// When
		val model1 = parser.parse(file1)
		val model2 = parser.parse(file2, model1.eResource.resourceSet)

		// Then
		model1.assertNoErrors
		model2.assertNoErrors

		// Verfiy object identity		
		val dialog = model1.componentTypes.assertSingleElement
		val myDialog = model2.components.assertSingleElement
		myDialog.type.assertSame(dialog)
		
		val template = model1.interactionTypes.assertSingleElement.template
		val sizeVariable = template.referenceableVariables.assertSingleElement
		myDialog.valueSpaceAssignments.assertSingleElement => [
			variable.assertSame(sizeVariable)
		]
	}
	
}