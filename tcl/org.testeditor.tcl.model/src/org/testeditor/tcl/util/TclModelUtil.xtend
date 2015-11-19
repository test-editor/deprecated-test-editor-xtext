package org.testeditor.tcl.util

import java.util.List
import java.util.Map
import javax.inject.Singleton
import org.testeditor.aml.model.ComponentElement
import org.testeditor.aml.model.InteractionType
import org.testeditor.aml.model.TemplateText
import org.testeditor.aml.model.TemplateVariable
import org.testeditor.tcl.StepContent
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.StepContentText
import org.testeditor.tcl.StepContentVariable
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.TestStepContext

@Singleton
class TclModelUtil {

	def String getName(TclModel model) {
		val lastSegment = model.eResource?.URI?.lastSegment
		if (lastSegment !== null) {
			val separator = lastSegment.lastIndexOf('.')
			if (separator >= 0) {
				return lastSegment.substring(0, separator).toFirstUpper
			} else {
				return lastSegment.toFirstUpper
			}
		}
		return null
	}

	def String restoreString(List<StepContent> contents) {
		return contents.map [
			switch (it) {
				StepContentVariable: '''"«value»"'''
				StepContentElement: '''<«value»>'''
				default:
					value
			}
		].join(' ')
	}

	def InteractionType getInteraction(TestStep step, TestStepContext context) {
		val component = context.component
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
				TemplateText: value
			}
		].join(' ')
		val normalizedStepContent = step.contents.map [
			switch (it) {
				StepContentElement: '<>'
				StepContentVariable: '""'
				StepContentText: value
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
		if (templateVariables.size !== stepContentVariables.size) {
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

}