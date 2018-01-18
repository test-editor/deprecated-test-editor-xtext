/*******************************************************************************
 * Copyright (c) 2012 - 2018 Signal Iduna Corporation and others.
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
package org.testeditor.rcp4.views.teststepselector

import javax.inject.Inject
import org.junit.Test
import org.testeditor.aml.AmlFactory
import org.testeditor.aml.TemplateContent
import org.testeditor.aml.dsl.tests.AbstractAmlTest

class AmlModelLabelProviderTest extends AbstractAmlTest {

	@Inject
	AmlModelLabelProvider amlModelLabelProvider

// TODO: get this injected!
//	@Inject
//	AmlFactory factory
	AmlFactory factory = AmlFactory.eINSTANCE

	@Test
	def void labelForModelIsPackage() {
		// given
		val amlModel = factory.createAmlModel => [^package = "test"]

		// when
		val result = amlModelLabelProvider.getText(amlModel)

		// then
		assertEquals(result, "test")
	}

	@Test
	def void labelForComponentIsName() {
		// given
		val component = factory.createComponent => [name = "test"]

		// when
		val result = amlModelLabelProvider.getText(component)

		// then
		assertEquals(result, "test")
	}

	@Test
	def void labelForComponentElementIsName() {
		// given
		val componentElement = factory.createComponentElement => [name = "test"]

		// when
		val result = amlModelLabelProvider.getText(componentElement)

		// then
		assertEquals(result, "test")
	}

	@Test
	def void labelForInteractionTypesIsTheTemplateString() {
		// given
		val interactionType = factory.createInteractionType => [
			template = factory.createTemplate => [
				contents += #[
					factory.createTemplateText => [value = "Insert Text"],
					factory.createTemplateVariable => [name = "text"],
					factory.createTemplateText => [value = "into"],
					factory.createTemplateVariable => [name = "element"]
				].map[it as TemplateContent]
			]
		]

		// when
		val result = amlModelLabelProvider.getText(interactionType)

		// then
		assertEquals(result, 'Insert Text "text" into <element>')
	}

}
