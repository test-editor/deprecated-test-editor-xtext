/*******************************************************************************
 * Copyright (c) 2012 - 2017 Signal Iduna Corporation and others.
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
import java.util.Set
import javax.inject.Inject
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.common.types.JvmTypeReference
import org.testeditor.aml.Template
import org.testeditor.aml.TemplateVariable
import org.testeditor.dsl.common.util.CollectionUtils
import org.testeditor.tcl.AssertionTestStep
import org.testeditor.tcl.AssignmentVariable
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.TestStepWithAssignment
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tsl.StepContent

class TclSimpleTypeUtils {
	@Inject SimpleTypeComputer simpleTypeComputer
	@Inject extension TclModelUtil  tclModelUtil
	@Inject extension CollectionUtils 

	/**
	 * provide an iterable with all step content variables as key and their respective fixture parameter type as value
	 */
	def Iterable<Pair<StepContent, Optional<JvmTypeReference>>> getStepVariableFixtureParameterTypePairs(TestStep step) {
		val result=newLinkedList
		val interaction=step.interaction
		if (interaction !== null ){
			val variablesWithTypes = simpleTypeComputer.getVariablesWithTypes(interaction)
			val callParameters = step.stepContentVariables
			val templateParameters = step.interaction.template.contents.filter(TemplateVariable)
			templateParameters.forEach[templateVariable,templateParameterIndex|
				result.add(new Pair(callParameters.get(templateParameterIndex), variablesWithTypes.get(templateVariable)))
			]				
		}
		return result
	}
	
	/** 
	 * get the actual jvm types from the fixtures that are transitively used and to which this variable/parameter is passed to
	 */
	def dispatch Set<Optional<JvmTypeReference>> getAllTypeUsagesOfVariable(ComponentTestStepContext componentTestStepContext, String variableName) {
		// type derivation of variable usage within assertions is not implemented "yet" => filter on test steps only
		// TODO this has to be implemented if the check is to be performed on assertions!
		val typesUsages = componentTestStepContext.steps.filter(TestStep).map [ step |
			step.stepVariableFixtureParameterTypePairs.filterKey(VariableReference).filter [
				key.variable.name == variableName
			].map[value]
		].flatten.filterNull.toSet
		return typesUsages
	}

	/** 
	 * get the actual jvm types from the fixtures that are transitively used and to which this variable/parameter is passed to.
	 * Since a parameter can be used in multiple parameter positions of subsequent fixture calls, the size of the set of types can be > 1
	 */
	def dispatch Set<Optional<JvmTypeReference>> getAllTypeUsagesOfVariable(MacroTestStepContext callingMacroTestStepContext,
		String variable) {
		callingMacroTestStepContext.steps.filter(TestStep).map[ step |
			val macroCalled = step.findMacroDefinition(callingMacroTestStepContext)
			if (macroCalled != null) {
				val templateParamToVarRefMap = mapCalledTemplateParamToCallingVariableReference(
					step, macroCalled.template, variable)
				val calledMacroTemplateParameters = templateParamToVarRefMap.values.map[name].toSet
				val contextsUsingAnyOfTheseParameters = macroCalled.contexts.filter [
					makesUseOfVariablesViaReference(calledMacroTemplateParameters)
				]
				val typesOfAllParametersUsed = contextsUsingAnyOfTheseParameters.map [ context |
					context.getAllTypeUsagesOfVariables(calledMacroTemplateParameters)
				].flatten.toSet
				return typesOfAllParametersUsed
			} else {
				return #{}
			}		
		].flatten.toSet
	}
	
	private def Iterable<Optional<JvmTypeReference>> getAllTypeUsagesOfVariables(TestStepContext context, Iterable<String> variables) {
		variables.map [ parameter |
			context.getAllTypeUsagesOfVariable(parameter)
		].flatten
	}

	private def Map<StepContent, TemplateVariable> mapCalledTemplateParamToCallingVariableReference(TestStep callingStep,
		Template calledMacroTemplate, String callingVariableReference) {
		val varMap = getStepContentToTemplateVariablesMapping(callingStep, calledMacroTemplate)
		return varMap.filter [ stepContent, templateVariable  |
			stepContent.makesUseOfVariablesViaReference(#{callingVariableReference})
		]
	}

	def JvmTypeReference determineType(AssignmentVariable assignmentVariable) {
		val testStep = EcoreUtil2.getContainerOfType(assignmentVariable, TestStep)
		return testStep.collectDeclaredVariablesTypeMap.values.head
	}

	/**
	 * collect all variables declared (e.g. through assignment)
	 */
	def dispatch Map<String, JvmTypeReference> collectDeclaredVariablesTypeMap(TestStepContext context) {
		val result = newHashMap
		context.steps.map[collectDeclaredVariablesTypeMap].forEach[result.putAll(it)]
		return result
	}
	
	def dispatch Map<String, JvmTypeReference> collectDeclaredVariablesTypeMap(TestStepWithAssignment testStep) {
		val typeReference = testStep.interaction?.defaultMethod?.operation?.returnType
		return #{testStep.variable.name -> typeReference}
	}
	
	def dispatch Map<String, JvmTypeReference> collectDeclaredVariablesTypeMap(TestStep testStep) {
		// TestSteps (not Assignments) do not introduce any variables 
		return emptyMap
	}
	
	def dispatch Map<String, JvmTypeReference> collectDeclaredVariablesTypeMap(AssertionTestStep testStep) {
		// Assertion test steps cannot contain variable declarations
		return emptyMap
	}
	
	
}