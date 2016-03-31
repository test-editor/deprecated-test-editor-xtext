/*******************************************************************************
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
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

import java.util.HashMap
import java.util.List
import java.util.Map
import javax.inject.Inject
import org.eclipse.xtext.validation.Check
import org.eclipse.xtext.xtype.XImportSection
import org.testeditor.tcl.AEVariableReference
import org.testeditor.tcl.AssertionExpression
import org.testeditor.tcl.AssertionTestStep
import org.testeditor.tcl.BinaryAssertionExpression
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.TclPackage
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.TestStepWithAssignment
import org.testeditor.tcl.impl.AssertionTestStepImpl
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tsl.SpecificationStep
import org.testeditor.tsl.StepContentVariable
import org.testeditor.tsl.TslPackage

class TclValidator extends AbstractTclValidator {

	public static val UNKNOWN_NAME = 'unknownName'
	public static val NO_VALID_IMPLEMENTATION = 'noValidImplementation'
	public static val INVALID_NAME = 'invalidName'
	public static val INVALID_MAP_REF = 'invalidMapReference'
	public static val VARIABLE_UNKNOWN_HERE = 'varUnknownHere'
	public static val VARIABLE_ASSIGNED_MORE_THAN_ONCE = 'varAssignedMoreThanOnce'
	public static val UNALLOWED_VALUE = 'unallowedValue'

	@Inject var extension TclModelUtil tclModelUtil

	@Check
	def void referencesComponentElement(StepContentElement contentElement) {
		val component = contentElement.componentElement
		if (component === null) {
			error('No ComponentElement found.', contentElement, null)
		}
	}

	override checkImports(XImportSection importSection) {
		// ignore for now
	}

	@Check
	def checkMaskPresent(TestStepContext tsContext) {
		if (tsContext.component.eIsProxy) {
			warning("mask is not defined in aml", TclPackage.Literals.TEST_STEP_CONTEXT__COMPONENT, UNKNOWN_NAME);
		}
	}

	@Check
	def checkVariableUsageWithinAssertionExpressions(TestStepContext tsContext) {
		val varTypeMap = new HashMap<String, String>
		// collect all var assignments
		tsContext.steps.forEach [it,index|
			if (it instanceof TestStepWithAssignment) {
				// check "in order" (to prevent variable usage before assignment)
				if (varTypeMap.containsKey(variableName)) {
					val message = '''Variable '«variableName»' is assigned more than once.'''
					warning(message, TclPackage.Literals.TEST_STEP_CONTEXT__STEPS, index, VARIABLE_ASSIGNED_MORE_THAN_ONCE);
				} else {
					varTypeMap.put(variableName, getInteraction.defaultMethod.operation.returnType.identifier)
				}
			} else if (it instanceof AssertionTestStepImpl) {
				executeCheckVariableUsageWithinAssertionExpressions(varTypeMap, index)
			}
		]
	}

	private def executeCheckVariableUsageWithinAssertionExpressions(AssertionTestStep step,
		Map<String, String> varTypeMap, int index) {
		step.expression.collectVariableUsage.forEach [
			if (varTypeMap.get(name) == null) { // regular variable dereference
				val message = '''Variable '«name»' is unknown here.'''
				error(message, eContainer, eContainingFeature, VARIABLE_UNKNOWN_HERE)
			} else if (key != null) { // dereference map with a key
				val typeIdentifier = varTypeMap.get(name).replaceFirst("<.*", "")
				if (typeIdentifier.
					isNotAssignableToMap) {
					val message = '''Variable '«name»' of type '«typeIdentifier»' does not implement '«Map.canonicalName»'. It cannot be used with key '«key»'.'''
					error(message, eContainer, eContainingFeature, INVALID_MAP_REF)
				}
			}
		]
	}

	private def isNotAssignableToMap(String typeIdentifier) {
		return !typeof(Map).isAssignableFrom(Class.forName(typeIdentifier))
	}

	private def Iterable<AEVariableReference> collectVariableUsage(AssertionExpression expression) {
		switch (expression) {
			BinaryAssertionExpression:
				return expression.left.collectVariableUsage + expression.right.collectVariableUsage
			AEVariableReference:
				return #[expression]
			default:
				return #[]
		}
	}

	@Check
	def checkSpec(TestCase testCase) {
		val specification = testCase.specification
		if (specification != null) {
			if (!specification.steps.matches(testCase.steps)) {
				val message = '''Test case does not implement its specification '«specification.name»'.'''
				warning(message, TclPackage.Literals.TEST_CASE__SPECIFICATION, NO_VALID_IMPLEMENTATION)
			}
		}
	}

	def boolean matches(List<SpecificationStep> specSteps, List<SpecificationStepImplementation> specImplSteps) {
		if (specSteps.size > specImplSteps.size) {
			return false
		}
		return specImplSteps.map[contents.restoreString].containsAll(specSteps.map[contents.restoreString])
	}

	@Check
	def checkTestName(TestCase testCase) {
		if (!getExpectedName(testCase).equals(testCase.name)) {
			val message = '''Test case name does not match '«testCase.eResource.URI.lastSegment»'.'''
			error(message, TclPackage.Literals.TEST_CASE__NAME, INVALID_NAME);
		}
	}

	@Check
	def checkValueInValueSpace(StepContentVariable stepContentVariable) {
		var valueSpace = stepContentVariable.valueSpaceAssignment.valueSpace
		if (!valueSpace.isValidValue(stepContentVariable.value)) {
			val message = '''Value is not allowed in this step. Allowed values: '«valueSpace»'.'''
			warning(message, TslPackage.Literals.STEP_CONTENT__VALUE, UNALLOWED_VALUE);
		}
	}

}
