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
package org.testeditor.tcl.dsl.validation

import java.util.Optional
import java.util.Set
import javax.inject.Inject
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.common.types.JvmTypeReference
import org.testeditor.aml.TemplateVariable
import org.testeditor.dsl.common.util.CollectionUtils
import org.testeditor.tcl.AssignmentVariable
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.dsl.jvmmodel.SimpleTypeComputer
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tsl.StepContent
import org.testeditor.tsl.StepContentText

/**
 * Functions for validating types for variable/parameter definition and usage
 */
class TclTypeValidationUtil {
	@Inject SimpleTypeComputer simpleTypeComputer
	@Inject extension TclModelUtil  tclModelUtil
	@Inject extension CollectionUtils 

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
	def Iterable<Pair<StepContent, Optional<JvmTypeReference>>> getStepVariableFixtureParameterTypePairs(TestStep step) {
		val result=newLinkedList
		val interaction=step.interaction
		if (interaction !== null ){
			val definitionParametersWithTypes = simpleTypeComputer.getVariablesWithTypes(interaction) // no different types possible => use simple type computer
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
		String variableName) {
		callingMacroTestStepContext.steps.filter(TestStep).map [ step |
			val macroCalled = step.findMacroDefinition(callingMacroTestStepContext)			
			if (macroCalled !== null) {
				val stepContentVariables = step.stepContentVariables
				val callParameterIndices=stepContentVariables.filter[!(it instanceof StepContentText)].indexed.filterValue(VariableReference).filter[value.variable.name==variableName].map[key]
				val calledMacroTemplateParameters = macroCalled.template.contents.filter(TemplateVariable).indexed.filter[pair|callParameterIndices.exists[it==pair.key]].map[value.name].toSet
				// cannot make use of simple type computer here, since simple type computer heeds no usage of parameter is different type positions!
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
	
	/**
	 * Get the list of types that the passed variables are used as.
	 * 
	 * E.g. if a variable A is used as 'long' and a variable B is used as 'String' the result will be #[long,String]
	 * This method is useful for getting all types for variables that are used in multiple parameter positions, 
	 * to collect the types expected for these different parameters.
	 */
	private def Iterable<Optional<JvmTypeReference>> getAllTypeUsagesOfVariables(TestStepContext context, Iterable<String> variables) {
		variables.map [ variable |
			context.getAllTypeUsagesOfVariable(variable)
		].flatten
	}

	/**
	 * get the type that will be returned of the fixture that will be the type of this assignment variable  
	 */
	def JvmTypeReference determineType(AssignmentVariable assignmentVariable) {
		val testStep = EcoreUtil2.getContainerOfType(assignmentVariable, TestStep)
		return simpleTypeComputer.collectDeclaredVariablesTypeMap(testStep).get(assignmentVariable.name) 
	}

	/**
	 * Get the template variable of the interaction that is the corresponding parameter for this step content, 
	 * given that the step content is part of a fixture call (interaction).
	 * 
	 * e.g. useful in combination with {@link SimpleTypeComputer#getVariablesWithTypes}
	 */
	def TemplateVariable getTemplateParameterForCallingStepContent(StepContent stepContent) {
		val testStep = EcoreUtil2.getContainerOfType(stepContent, TestStep)
		val callParameterIndex = testStep.stepContentVariables.indexOfFirst(stepContent)
		val interaction = testStep.interaction
		val templateParameters = interaction?.template?.contents?.filter(TemplateVariable)
		if (interaction !== null //
			&& templateParameters !== null //
			&& templateParameters.length > callParameterIndex) {
			return templateParameters.drop(callParameterIndex).head
		}
		return null		
	}
	
}