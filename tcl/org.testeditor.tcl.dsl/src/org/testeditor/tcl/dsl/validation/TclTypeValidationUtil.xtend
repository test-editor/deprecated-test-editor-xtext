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

import javax.inject.Inject
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.common.types.JvmTypeReference
import org.testeditor.aml.TemplateContainer
import org.testeditor.aml.TemplateVariable
import org.testeditor.dsl.common.util.CollectionUtils
import org.testeditor.tcl.AssignmentVariable
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.dsl.jvmmodel.SimpleTypeComputer
import org.testeditor.tcl.dsl.jvmmodel.VariableCollector
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tsl.StepContent
import java.util.Optional

/**
 * Functions for validating types for variable/parameter definition and usage
 */
class TclTypeValidationUtil {
	@Inject SimpleTypeComputer simpleTypeComputer
	@Inject VariableCollector variableCollector
	@Inject extension TclModelUtil  tclModelUtil
	@Inject extension CollectionUtils 


	/** 
	 * get the type that this stepContent is expected to have in order to satisfy the parameter type of its transitively called fixture  
	 */
	def Optional<JvmTypeReference> getExpectedType(StepContent stepContent, TemplateContainer templateContainer)  {
		val parameterTypeMap = simpleTypeComputer.getVariablesWithTypes(templateContainer)
		val templateParameter = getTemplateParameterForCallingStepContent(stepContent)
		val expectedType = parameterTypeMap.get(templateParameter)
		if (!expectedType.present) {
			throw new RuntimeException("Unknown type")
		}
		return expectedType
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

	/**
	 * Get the template variable of the interaction that is the corresponding parameter for this step content, 
	 * given that the step content is part of a fixture call (interaction).
	 * 
	 * e.g. useful in combination with {@link SimpleTypeComputer#getVariablesWithTypes}
	 */
	def TemplateVariable getTemplateParameterForCallingStepContent(StepContent stepContent) {
		val testStep = EcoreUtil2.getContainerOfType(stepContent, TestStep)
		val callParameterIndex = testStep.stepContentVariables.indexOfFirst(stepContent)
		val templateContainer = testStep.templateContainer
		val templateParameters = templateContainer?.template?.contents?.filter(TemplateVariable)
		if (templateContainer !== null //
			&& templateParameters !== null //
			&& templateParameters.length > callParameterIndex) {
			return templateParameters.drop(callParameterIndex).head
		}
		return null		
	}
	
}