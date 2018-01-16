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
import javax.inject.Inject
import org.eclipse.xtext.common.types.JvmTypeReference
import org.testeditor.dsl.common.util.CollectionUtils
import org.testeditor.tcl.AssertionTestStep
import org.testeditor.tcl.AssignmentThroughPath
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.TestStepWithAssignment
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tcl.StepContainer

class VariableCollector {
	
	@Inject extension TclModelUtil
	@Inject extension CollectionUtils
	
	/**
	 * collect all variables (and their types) declared (currently only through assignment).
	 * the assignment fixes the type of the variable.
	 */
	def dispatch Map<String, JvmTypeReference> collectDeclaredVariablesTypeMap(StepContainer container) {
		return container.contexts.map[collectDeclaredVariablesTypeMap].mergeOverwriting
	}

	def dispatch Map<String, JvmTypeReference> collectDeclaredVariablesTypeMap(TestStepContext context) {
		return context.steps.map[collectDeclaredVariablesTypeMap].mergeOverwriting
	}
	
	def dispatch Map<String, JvmTypeReference> collectDeclaredVariablesTypeMap(AssignmentThroughPath assignment) {
		// Map entry assignments use a known map, no new variables are declared
		return emptyMap
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
	
}