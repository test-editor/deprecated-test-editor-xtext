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

import com.google.gson.JsonObject
import java.util.Map
import java.util.Optional
import java.util.Set
import javax.inject.Inject
import org.eclipse.xtext.common.types.JvmTypeReference
import org.testeditor.aml.TemplateVariable
import org.testeditor.aml.Variable
import org.testeditor.dsl.common.util.CollectionUtils
import org.testeditor.tcl.AssignmentThroughPath
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.VariableReferencePathAccess
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tsl.StepContentText

/**
 * compute all type usages of certain model elements
 */
class TclTypeUsageComputer {

	@Inject SimpleTypeComputer simpleTypeComputer
	@Inject extension TclModelUtil  tclModelUtil
	@Inject extension CollectionUtils 

	/** 
	 * get the actual jvm types from the fixtures that are transitively used and to which this variable/parameter is passed to
	 */
	def dispatch Set<Optional<JvmTypeReference>> getAllPossibleTypeUsagesOfVariable(ComponentTestStepContext componentTestStepContext, String variableName) {
		// type derivation of variable usage within assertions is not implemented "yet" => filter on test steps only
		// TODO this has to be implemented if the check is to be performed on assertions!
		val typesUsages = componentTestStepContext.steps.filter(TestStep).map [ step |
			simpleTypeComputer.getStepVariableFixtureParameterTypePairs(step).filterKey(VariableReference).filter[!(key instanceof VariableReferencePathAccess)].filter [
				key.variable.name == variableName
			].map[value]
		].flatten.filterNull
		val typesUsagesThroughAssignments = componentTestStepContext.steps.filter(AssignmentThroughPath).filter[
			variableReference.variable.name == variableName
		].map [ step |
			step.possibleAssignmentTypes.map[value]
		].flatten
		return (typesUsages + typesUsagesThroughAssignments).toSet
	}

	/** 
	 * get the actual jvm types from the fixtures that are transitively used and to which this variable/parameter is passed to.
	 * Since a parameter can be used in multiple parameter positions of subsequent fixture calls, the size of the set of types can be > 1
	 */
	def dispatch Set<Optional<JvmTypeReference>> getAllPossibleTypeUsagesOfVariable(MacroTestStepContext callingMacroTestStepContext,
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
			context.getAllPossibleTypeUsagesOfVariable(variable)
		].flatten
	}
	
	/** TODO: the assignment of values may either be used for maps or for json objects. deciding its usage here is not applicable */
	def Iterable<Pair<Variable, Optional<JvmTypeReference>>> getPossibleAssignmentTypes(AssignmentThroughPath assignment) {
		val variableReference = assignment.getVariableReference
		val jsonObjectType = JsonObject.getJvmTypeReferenceForClass(assignment)
		val mapType = Map.getJvmTypeReferenceForClass(assignment)
		val typeList = #[ jsonObjectType, mapType ]
		return typeList.map[new Pair(variableReference.variable, Optional.of(it))]
	}
	
}