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
package org.testeditor.tcl.util

import java.util.List
import java.util.Map
import javax.inject.Singleton
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.InteractionType
import org.testeditor.aml.TemplateText
import org.testeditor.aml.TemplateVariable
import org.testeditor.aml.ValueSpaceAssignment
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.TestStep
import org.testeditor.tsl.SpecificationStep
import org.testeditor.tsl.StepContent
import org.testeditor.tsl.StepContentText
import org.testeditor.tsl.StepContentVariable
import org.testeditor.tsl.util.TslModelUtil

@Singleton
class TclModelUtil extends TslModelUtil {

	override String restoreString(List<StepContent> contents) {
		return contents.map [
			switch (it) {
				StepContentVariable: '''"«value»"'''
				StepContentElement: '''<«value»>'''
				default:
					value
			}
		].join(' ')
	}

	def InteractionType getInteraction(TestStep step) {
		// TODO this should be solved by using an adapter (so that we don't need to recalculate it over and over again)
		val component = step.context.component
		if (component !== null) {
			val allElementInteractions = component.elements.map[type.interactionTypes].flatten.filterNull
			val interactionTypes = component.type.interactionTypes + allElementInteractions
			return interactionTypes.findFirst[matches(step)]
		}
		return null
	}

	protected def boolean matches(InteractionType interaction, TestStep step) {
		val normalizedTemplate = interaction.template.contents.map [
			switch (it) {
				TemplateVariable case name == 'element': '<>'
				TemplateVariable: '""'
				TemplateText: value.trim
			}
		].join(' ')
		val normalizedStepContent = step.contents.map [
			switch (it) {
				StepContentElement: '<>'
				StepContentVariable: '""'
				StepContentText: value.trim
			}
		].join(' ')
		return normalizedTemplate == normalizedStepContent
	}

	// TODO we need a common super class for StepContentElement and StepContentVariable
	def Map<TemplateVariable, StepContent> getVariableToValueMapping(TestStep step, InteractionType interaction) {
		val map = newHashMap
		val templateVariables = interaction.template.contents.filter(TemplateVariable).toList
		val stepContentVariables = step.contents.filter [
			it instanceof StepContentElement || it instanceof StepContentVariable
		].toList
		if (templateVariables.size !==
			stepContentVariables.
				size) {
					throw new IllegalArgumentException('''Variables for '«step.contents.restoreString»' did not match the parameters of interaction '«interaction.name»'.''')
				}
				for (var i = 0; i < templateVariables.size; i++) {
					map.put(templateVariables.get(i), stepContentVariables.get(i))
				}
				return map
			}

			def ComponentElement getComponentElement(StepContentElement contentElement) {
				val container = contentElement.eContainer
				if (container instanceof TestStep) {
					val component = container.context.component
					return component.elements.findFirst[name == contentElement.value]
				}
				return null
			}

			def ValueSpaceAssignment getValueSpaceAssignment(StepContentVariable contentElement) {
				val container = contentElement.eContainer
				if (container instanceof TestStep) {
					val component = container.context.component
					val valueSpace = getValueSpaceAssignment(component, container)
					if (valueSpace != null) {
						return valueSpace
					}
				}
				return null
			}

			def ValueSpaceAssignment getValueSpaceAssignment(Component component, TestStep container) {
				for (element : component.elements) {
					val valueSpace = getValueSpaceAssignment(element, container)
					if (valueSpace != null) {
						return valueSpace
					}
				}
				return null
			}

			def ValueSpaceAssignment getValueSpaceAssignment(ComponentElement element, TestStep container) {
				val foo = element.valueSpaceAssignments
				return foo.findFirst[variable.template.interactionType.name == container.interaction.name]
			}

			def SpecificationStep getSpecificationStep(SpecificationStepImplementation stepImplementation) {
				val tslModel = stepImplementation.test.specification
				if (tslModel !== null) {
					return tslModel.steps.findFirst[matches(stepImplementation)]
				}
				return null
			}

		}
		