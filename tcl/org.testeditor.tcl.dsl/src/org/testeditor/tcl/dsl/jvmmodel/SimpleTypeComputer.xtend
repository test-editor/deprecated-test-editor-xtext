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
package org.testeditor.tcl.dsl.jvmmodel

import java.util.Map
import java.util.Optional
import javax.inject.Inject
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.common.types.JvmEnumerationType
import org.eclipse.xtext.common.types.JvmTypeReference
import org.testeditor.aml.InteractionType
import org.testeditor.aml.MethodReference
import org.testeditor.aml.TemplateContainer
import org.testeditor.aml.TemplateVariable
import org.testeditor.dsl.common.util.CollectionUtils
import org.testeditor.tcl.AssignmentVariable
import org.testeditor.tcl.Macro
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tsl.StepContent

import static java.util.Optional.*

/**
 * A simple and not very clever type computer for used variables in test steps.
 */
class SimpleTypeComputer {

	@Inject extension TclModelUtil
	@Inject extension CollectionUtils
	@Inject VariableCollector variableCollector

	/**
	 * get a map of parameters (TemplateVariable) to it (expected) type
	 */
	def dispatch Map<TemplateVariable, Optional<JvmTypeReference>> getVariablesWithTypes(InteractionType interaction) {
		return interaction.defaultMethod.interactionTemplateVariablesToMethodParameterTypesMapping
	}

	def dispatch Map<TemplateVariable, Optional<JvmTypeReference>> getVariablesWithTypes(Macro macro) {
		val result = newLinkedHashMap

		// Get macro parameter variables and put them in the result without a type (Optional.empty)
		val variables = macro.template.contents.filter(TemplateVariable)
		result.putAll(variables.toInvertedMap[empty])

		// Get variable usages with their types, last usage wins (validation should make sure type usage is consistent)
		for (context : macro.contexts) {
			val stepsWithVariableReferences = context.steps.filter(TestStep).filter[!contents.filter(VariableReference).empty]
			for (step : stepsWithVariableReferences) {
				result.putAll(getVariablesWithTypes(step, variables))
			}
		}
		return result
	}

	/**
	 * provide an iterable with all step content variables as key and their respective fixture parameter type as value
	 *
	 * be aware of the fact that a call expecting two parameters, passing the same into both of them
	 * will result in a second pair within this list, with possible different type references!
	 * e.g.
	 *    given: template definition: template = "do something with" ${param1} "and" ${param2}
	 *           pointing to a fixture method that has a String and a long parameter respectively
	 *           environment variable: require envParam
	 *           test step: - do something with \@envParam and \@envParam
	 *    will result in the following iterable
	 *      #[<EnvironmenVariableReference(envParam), String>, <EnvironmentVariableReference(envParam), long>]
	 *
	 * this is important for validation purposes.
	 * the simple type computer does not heed the case that a variable or parameter may be used with two different types.
	 */
	def Iterable<Pair<StepContent, Optional<JvmTypeReference>>> getStepVariableFixtureParameterTypePairs(
		TestStep step) {
		val result = newLinkedList
		val interaction = step.interaction
		if (interaction !== null) {
			val definitionParametersWithTypes = getVariablesWithTypes(interaction) // no different types possible => use simple type computer
			val callParameters = step.stepContentVariables
			val templateParameters = step.interaction.template.contents.filter(TemplateVariable)
			templateParameters.forEach [ templateVariable, templateParameterIndex |
				result += new Pair(callParameters.get(templateParameterIndex),
					definitionParametersWithTypes.get(templateVariable))
			]
		}
		return result
	}

	/**
	 * get the type that this stepContent is expected to have in order to satisfy the parameter type of its transitively called fixture
	 * 
	 * ensures that non null is returned
	 */
	def Optional<JvmTypeReference> getExpectedType(StepContent stepContent, TemplateContainer templateContainer) {
		val templateParameter = getTemplateParameterForCallingStepContent(stepContent)
		return getExpectedType(templateParameter, templateContainer)
	}

	/**
	 * get the type that this template variable is expected to have in order to satisfy the parameter type of its transitively called fixture.
	 * 
	 * ensures that non null is returned
	 */
	def Optional<JvmTypeReference> getExpectedType(TemplateVariable templateParameter, TemplateContainer templateContainer) {
		val parameterTypeMap = getVariablesWithTypes(templateContainer)
		val expectedType = parameterTypeMap.get(templateParameter)
		return expectedType?:empty
	}

	/**
	 * get the type that will be returned of the fixture that will be the type of this assignment variable
	 */
	def JvmTypeReference determineType(AssignmentVariable assignmentVariable) {
		val testStep = EcoreUtil2.getContainerOfType(assignmentVariable, TestStep)
		val result = variableCollector.collectDeclaredVariablesTypeMap(testStep).get(assignmentVariable.name)
		if (result === null) {
			throw new RuntimeException('''Could not find type for variable = '«assignmentVariable.name»'.''')
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
					val optionalType = templateVariableToType.get(stepContentToTemplateVariable.get(variableReference))
					result.put(variable, optionalType?:empty)
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
