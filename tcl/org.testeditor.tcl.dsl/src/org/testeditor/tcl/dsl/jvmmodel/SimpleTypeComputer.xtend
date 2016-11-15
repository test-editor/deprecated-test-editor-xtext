package org.testeditor.tcl.dsl.jvmmodel

import java.util.Map
import java.util.Optional
import javax.inject.Inject
import org.eclipse.xtext.common.types.JvmTypeReference
import org.testeditor.aml.InteractionType
import org.testeditor.aml.MethodReference
import org.testeditor.aml.TemplateVariable
import org.testeditor.tcl.Macro
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.util.TclModelUtil

import static java.util.Optional.*

/**
 * A simple and not very clever type computer for used variables in test steps.
 */
class SimpleTypeComputer {

	@Inject extension TclModelUtil

	def dispatch Map<TemplateVariable, Optional<JvmTypeReference>> getVariablesWithTypes(InteractionType interaction) {
		return interaction.defaultMethod.interactionTemplateVariablesToMethodParameterTypesMapping
	}

	def dispatch Map<TemplateVariable, Optional<JvmTypeReference>> getVariablesWithTypes(Macro macro) {
		val result = newLinkedHashMap

		// Get variables and put them in the result without a type (null)
		val variables = macro.template.contents.filter(TemplateVariable)
		result.putAll(variables.toInvertedMap[empty])

		// Get variable usages with their types, last usage wins (validation should make sure it's only used once)
		for (context : macro.contexts) {
			val stepsWithVariableReferences = context.steps.filter(TestStep).filter[!contents.filter(VariableReference).empty]
			for (step : stepsWithVariableReferences) {
				result.putAll(getVariablesWithTypes(step, variables))
			}
		}
		return result
	}

	private def Map<TemplateVariable, Optional<JvmTypeReference>> getVariablesWithTypes(TestStep step, Iterable<TemplateVariable> variables) {
		val result = newLinkedHashMap
		val interactionOrMacro = step.findInteractionOrMacro
		if (interactionOrMacro !== null) {
			val templateVariableToType = getVariablesWithTypes(interactionOrMacro)
			val stepContentToTemplateVariable = getStepContentToTemplateVariablesMapping(step, interactionOrMacro.template)
			for (variable : variables) {
				val variableReference = stepContentToTemplateVariable.keySet.filter(VariableReference).findFirst[it.variable == variable]
				if (variableReference !== null) {
					result.put(variable, templateVariableToType.get(stepContentToTemplateVariable.get(variableReference)))
				} // else: no type found, will remain Object (either unused variable or nested interaction / macro was not found)
			}

		}
		return result
	}

	private def Map<TemplateVariable, Optional<JvmTypeReference>> getInteractionTemplateVariablesToMethodParameterTypesMapping(
		MethodReference methodReference) {
		val operationParameters = methodReference.operation.parameters
		val map = newHashMap
		methodReference.parameters.forEach [ parameter, i |
			val operationParameter = operationParameters.get(i)
			map.put(parameter, ofNullable(operationParameter.parameterType))
		]
		return map
	}

}
