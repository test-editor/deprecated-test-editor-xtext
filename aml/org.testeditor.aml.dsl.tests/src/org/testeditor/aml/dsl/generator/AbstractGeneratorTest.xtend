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
package org.testeditor.aml.dsl.generator

import org.testeditor.aml.AmlFactory
import org.testeditor.aml.ComponentElementType
import org.testeditor.aml.dsl.tests.AbstractAmlTest

abstract class AbstractGeneratorTest extends AbstractAmlTest {

	protected static val factory = AmlFactory.eINSTANCE

	def createAmlModel() {
		return factory.createAmlModel
	}

	def createComponentType(String name) {
		return factory.createComponentType => [
			it.name = name
		]
	}

	def createComponent(String name) {
		return factory.createComponent => [
			it.name = name
		]
	}
	
	def createElementType(String name) {
		return factory.createComponentElementType => [
			it.name = name
		]
	}
	
	def createElement(String name, ComponentElementType type) {
		return factory.createComponentElement => [
			it.name = name
			it.type = type
		]
	}
	
	def createInteractionType(String name) {
		createInteractionType(name, null)
	}
	
	def createInteractionType(String name, String label) {
		return factory.createInteractionType => [
			it.name = name
			it.label = label
		]
	}

	def createTemplateText(String value) {
		return factory.createTemplateText => [
			it.value = value
		]
	}

	def createTemplateVariable(String name) {
		return factory.createTemplateVariable => [
			it.name = name
		]
	}

}