package org.testeditor.tcl.util

import java.util.Optional
import com.google.inject.Inject
import org.testeditor.aml.ElementWithInteractions
import org.testeditor.aml.TemplateVariable
import org.testeditor.aml.ValueSpace
import org.testeditor.aml.ValueSpaceAssignment
import org.testeditor.aml.ValueSpaceAssignmentContainer
import org.testeditor.tcl.TestStep
import org.testeditor.tsl.StepContentVariable

import static java.util.Optional.ofNullable

import static extension org.eclipse.xtext.EcoreUtil2.getContainerOfType

/**
 * Helper class for retrieving the {@link ValueSpace} for a given {@link StepContentVariable}.
 */
class ValueSpaceHelper {

	@Inject extension TclModelUtil

	def Optional<ValueSpace> getValueSpace(StepContentVariable variable) {
		return ofNullable(getValueSpaceAssignment(variable)?.valueSpace)
	}

	private def ValueSpaceAssignment getValueSpaceAssignment(StepContentVariable stepContent) {
		val testStep = stepContent.getContainerOfType(TestStep)
		val templateVariable = getTemplateVariable(testStep, stepContent)
		if (templateVariable !== null) {
			// need to check which kind of interaction we have here (on a component element, component or macro)
			val componentElement = testStep.componentElement
			if (componentElement !== null) {
				// interaction on a component element
				return getValueSpaceAssignment(componentElement, templateVariable)
			} else {
				// could be a component or a macro here, currently we only support components
				val component = testStep.componentContext?.component
				if (component !== null) {
					return getValueSpaceAssignment(component, templateVariable)
				}
			}

		} // else: without a template variable we cannot determine any value space assignments
		return null
	}

	private def ValueSpaceAssignment getValueSpaceAssignment(ElementWithInteractions<?> element, TemplateVariable variable) {
		// First, try to check if the element itself has value space assignments
		val elementAssignment = element.valueSpaceAssignments.findFirst[it.variable == variable]
		if (elementAssignment !== null) {
			return elementAssignment
		}

		// Next, check if the type of the element has value space assignments
		if (element.type !== null) {
			val typeAssignment = element.type.valueSpaceAssignments.findFirst[it.variable == variable]
			if (typeAssignment !== null) {
				return typeAssignment
			}
		}

		// Finally, check if the container of the template variable has value space assignments
		val variableContainer = variable.getContainerOfType(ValueSpaceAssignmentContainer)
		if (variableContainer !== null) {
			val containerAssignment = variableContainer.valueSpaceAssignments.findFirst[it.variable == variable]
			if (containerAssignment !== null) {
				return containerAssignment
			}
		}

		return null
	}

	private def TemplateVariable getTemplateVariable(TestStep testStep, StepContentVariable stepContent) {
		val template = testStep.findInteractionOrMacro?.template
		if (template !== null) {
			val mapping = getStepContentToTemplateVariablesMapping(testStep, template)
			return mapping.get(stepContent)
		}
		return null
	}

}
