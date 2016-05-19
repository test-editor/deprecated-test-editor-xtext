package org.testeditor.tml.util

import java.util.List
import java.util.Map
import java.util.Set
import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.InteractionType
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.Template
import org.testeditor.aml.TemplateText
import org.testeditor.aml.TemplateVariable
import org.testeditor.aml.ValueSpaceAssignment
import org.testeditor.tml.ComponentTestStepContext
import org.testeditor.tml.Macro
import org.testeditor.tml.MacroTestStepContext
import org.testeditor.tml.StepContentDereferencedVariable
import org.testeditor.tml.StepContentElement
import org.testeditor.tml.TestStep
import org.testeditor.tsl.StepContent
import org.testeditor.tsl.StepContentText
import org.testeditor.tsl.StepContentVariable
import org.testeditor.tsl.util.TslModelUtil

class TmlModelUtil extends TslModelUtil {
	@Inject extension ModelUtil

	override String restoreString(List<StepContent> contents) {
		return contents.map [
			switch (it) {
				StepContentVariable: '''"«value»"'''
				StepContentElement: '''<«value»>'''
				StepContentDereferencedVariable: '''@«value»'''
				default:
					value
			}
		].join(' ')
	}

	def Macro findMacroDefinition(MacroTestStepContext macroCallSite) {
		macroCallSite.macroModel.macros?.findFirst[template.normalize == macroCallSite.step.normalize]
	}

	def InteractionType getInteraction(TestStep step) {
		// TODO this should be solved by using an adapter (so that we don't need to recalculate it over and over again)
		val component = step.componentContext?.component
		if (component !== null) {
			val allElementInteractions = component.elements.map[type.interactionTypes].flatten.filterNull
			val interactionTypes = component.type.interactionTypes + allElementInteractions
			return interactionTypes.findFirst[matches(step)]
		}
		return null
	}

	def String normalize(Template template) {
		val normalizedTemplate = template.contents.map [
			switch (it) {
				TemplateVariable case name == 'element': '<>'
				TemplateVariable: '""'
				TemplateText: value.trim
			}
		].join(' ')
		return normalizedTemplate
	}

	def String normalize(TestStep step) {
		val normalizedStepContent = step.contents.map [
			switch (it) {
				StepContentElement: '<>'
				StepContentVariable: '""'
				StepContentDereferencedVariable: '""'
				StepContentText: value.trim
			}
		].join(' ')
		return normalizedStepContent
	}

	protected def boolean matches(InteractionType interaction, TestStep step) {
		return interaction.template.normalize == step.normalize
	}

	// TODO we need a common super class for StepContentElement and StepContentVariable and StepContentDereferencedVariable
	def Map<TemplateVariable, StepContent> getVariableToValueMapping(TestStep step, Template template) {
		val map = newHashMap
		val templateVariables = template.contents.filter(TemplateVariable).toList
		val stepContentVariables = step.contents.filter [
			it instanceof StepContentElement || it instanceof StepContentVariable ||
				it instanceof StepContentDereferencedVariable
		].toList
		if (templateVariables.size !== stepContentVariables.size) {
			val message = '''Variables for '«step.contents.restoreString»' did not match the parameters of template '«template.normalize»' (normalized).'''
			throw new IllegalArgumentException(message)
		}
		for (var i = 0; i < templateVariables.size; i++) {
			map.put(templateVariables.get(i), stepContentVariables.get(i))
		}
		return map
	}

	def ComponentElement getComponentElement(StepContentElement contentElement) {
		val container = contentElement.eContainer
		if (container instanceof TestStep) {
			val component = container.componentContext?.component
			return component?.elements?.findFirst[name == contentElement.value]
		}
		return null
	}

	def Template getMacroTemplate(StepContentElement contentElement) {
		val container = contentElement.eContainer
		if (container instanceof TestStep) {
			val macro = container.macroContext?.findMacroDefinition
			return macro?.template
		}
		return null
	}

	def ValueSpaceAssignment getValueSpaceAssignment(StepContentVariable contentElement) {
		val container = contentElement.eContainer
		if (container instanceof TestStep) {
			val component = container.componentContext?.component
			if (component != null) {
				val valueSpace = getValueSpaceAssignment(component, container)
				if (valueSpace != null) {
					return valueSpace
				}
			}
		}
		return null
	}

	def boolean hasComponentContext(TestStep step) {
		return step.componentContext != null
	}

	def ComponentTestStepContext getComponentContext(TestStep step) {
		if (step.eContainer instanceof ComponentTestStepContext) {
			return step.eContainer as ComponentTestStepContext
		} else {
			return null
		}
	}

	def boolean hasMacroContext(TestStep step) {
		return step.macroContext != null
	}

	def MacroTestStepContext getMacroContext(TestStep step) {
		if (step.eContainer instanceof MacroTestStepContext) {
			return step.eContainer as MacroTestStepContext
		} else {
			return null
		}
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
		return foo.findFirst[variable.template.interactionType.name == container.interaction?.name]
	}

	def Set<TemplateVariable> getMacroParameters(EObject object) {
		var curObject = object
		while (curObject != null) {
			if (curObject instanceof Macro) {
				return curObject.template.referenceableVariables
			}
			curObject = curObject.eContainer
		}
		return #{}
	}

}
