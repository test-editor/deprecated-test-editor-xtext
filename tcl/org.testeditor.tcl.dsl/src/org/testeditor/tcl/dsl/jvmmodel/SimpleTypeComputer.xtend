package org.testeditor.tcl.dsl.jvmmodel

import java.util.Map
import java.util.Optional
import com.google.inject.Inject
import org.eclipse.xtext.common.types.JvmEnumerationType
import org.eclipse.xtext.common.types.JvmTypeReference
import org.testeditor.aml.InteractionType
import org.testeditor.aml.MethodReference
import org.testeditor.aml.TemplateVariable
import org.testeditor.dsl.common.util.CollectionUtils
import org.testeditor.tcl.AssertionTestStep
import org.testeditor.tcl.Macro
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.TestStepWithAssignment
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.util.TclModelUtil

import static java.util.Optional.*

/**
 * A simple and not very clever type computer for used variables in test steps.
 */
class SimpleTypeComputer {

	@Inject extension TclModelUtil
	@Inject extension CollectionUtils

	/**
	 * get a map of parameters (TemplateVariable) to it (expected) type
	 */
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
	
	/**
	 * collect all variables (and their types) declared (e.g. through assignment)
	 */
	def dispatch Map<String, JvmTypeReference> collectDeclaredVariablesTypeMap(TestStepContext context) {
		val result = newHashMap
		context.steps.map[collectDeclaredVariablesTypeMap].forEach[result.putAll(it)]
		return result
	}
	
	def dispatch Map<String, JvmTypeReference> collectDeclaredVariablesTypeMap(TestStepWithAssignment testStep) {
		// must be declared before dispatch method collectDeclaredVariablesTypeMap(TestStep)
		val typeReference = testStep.interaction?.defaultMethod?.operation?.returnType
		return #{testStep.variable.name -> typeReference}
	}
	
	def dispatch Map<String, JvmTypeReference> collectDeclaredVariablesTypeMap(TestStep testStep) {
		// TestSteps (not TestStepAssignments) do not introduce any variables 
		return emptyMap
	}
	
	def dispatch Map<String, JvmTypeReference> collectDeclaredVariablesTypeMap(AssertionTestStep testStep) {
		// Assertion test steps cannot contain variable declarations
		return emptyMap
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
		if (!operationParameters.empty) {
			val elementIndex = methodReference.parameters.indexOfFirst[name == "element"]
			val parameterCountDiffersByOne = (operationParameters.size - methodReference.parameters.size == 1)
			val hasLocatorStrategy = parameterCountDiffersByOne //
					&& elementIndex >= 0 //
					&& operationParameters.get(elementIndex + 1).parameterType.type instanceof JvmEnumerationType
			methodReference.parameters.forEach [ parameter, methodCallParameterIndex |
				val methodDefinitionParameterIndex = if (hasLocatorStrategy && methodCallParameterIndex > elementIndex) {
						methodCallParameterIndex + 1 // when behind "element", and locatorStrategy is present
					} else {
						methodCallParameterIndex
					}
				val operationParameter = operationParameters.get(methodDefinitionParameterIndex)
				map.put(parameter, ofNullable(operationParameter.parameterType))
			]
		}
		return map
	}

}
