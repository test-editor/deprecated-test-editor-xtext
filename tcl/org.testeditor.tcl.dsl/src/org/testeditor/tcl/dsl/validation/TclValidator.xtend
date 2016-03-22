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

import java.util.List
import javax.inject.Inject
import org.eclipse.xtext.validation.Check
import org.eclipse.xtext.xtype.XImportSection
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.TclPackage
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tsl.SpecificationStep
import org.testeditor.tsl.StepContentVariable
import org.testeditor.tsl.TslPackage

class TclValidator extends AbstractTclValidator {

	public static val UNKNOWN_NAME = 'unknownName'
	public static val NO_VALID_IMPLEMENTATION = 'noValidImplementation'
	public static val INVALID_NAME = 'invalidName'
	public static val UNALLOWED_VALUE = 'unallowedValue'

	@Inject extension TclModelUtil

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
