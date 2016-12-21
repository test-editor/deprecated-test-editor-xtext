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
package org.testeditor.tcl.dsl.ui.tests.editor

import com.google.common.base.Strings
import com.google.common.io.ByteStreams
import java.nio.charset.StandardCharsets
import javax.inject.Inject
import org.junit.Before
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.InteractionType
import org.testeditor.dsl.common.testing.DslParseHelper
import org.testeditor.tcl.AbstractTestStep
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import org.testeditor.tcl.dsl.ui.editor.DropUtils

class AbstractTclModelDragAndDropUpdaterTest extends AbstractTclTest {

	@Inject protected extension DslParseHelper

	@Inject DropUtils dropUtils
	var AmlModel amlModel

	@Before
	def void parseAmlModel() {
		val encoded = ByteStreams.toByteArray(class.classLoader.getResourceAsStream("test.aml"))
		amlModel = parseAml(new String(encoded, StandardCharsets.UTF_8))
	}

	def protected ComponentTestStepContext createDroppedTestStepContext(String componentName, String interacionTypeName) {
		val component = amlModel.getComponent(componentName)
		val interactionType = component.getInteractionType(interacionTypeName)

		return dropUtils.createDroppedTestStepContext(component, null, interactionType)
	}

	def protected ComponentTestStepContext createDroppedTestStepContext(String componentName, String componentElementName,
		String interacionTypeName) {
		val component = amlModel.getComponent(componentName)
		val componentElement = component.getComponentElement(componentElementName)
		val interactionType = componentElement.getInteractionType(interacionTypeName)

		return dropUtils.createDroppedTestStepContext(component, componentElement, interactionType)
	}

	def protected Component getComponent(AmlModel amlModel, String componentName) {
		return amlModel.components.findFirst[name == componentName].assertNotNull
	}

	def protected InteractionType getInteractionType(Component component, String interactionTypeName) {
		return component.type.interactionTypes.findFirst[name == interactionTypeName].assertNotNull
	}

	def protected InteractionType getInteractionType(ComponentElement componentElement, String interactionTypeName) {
		return componentElement.type.interactionTypes.findFirst[name == interactionTypeName].assertNotNull
	}

	def protected ComponentElement getComponentElement(Component component, String elementName) {
		return component.elements.findFirst[name == elementName].assertNotNull
	}

	def protected ComponentTestStepContext getTestStepContext(TclModel tclModel, String componentTestStepName) {
		val contexts = tclModel.test.steps.head.contexts
		return contexts.filter(ComponentTestStepContext).findFirst[component.name == componentTestStepName].assertNotNull		
	}

	def protected AbstractTestStep getTestStep(TclModel tclModel, String componentTestStepName, int position) {
		val context = tclModel.getTestStepContext(componentTestStepName)
		return context.steps.get(position)
	}

	def protected String indent(CharSequence input, int level) {
		val indentation = Strings.repeat('\t', level)
		val value = input.toString.replaceAll('(?m)^(?!\r?\n)', indentation)
		return value
	}

}
