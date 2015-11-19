/*******************************************************************************
 * Copyright (c) 2012 - 2015 Signal Iduna Corporation and others.
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

import org.eclipse.xtext.validation.Check
import org.eclipse.xtext.xtype.XImportSection
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.TclPackage
import org.testeditor.tcl.TestStepContext
import javax.inject.Inject
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tcl.TestCase
import org.testeditor.tsl.SpecificationStep
import org.testeditor.tcl.SpecificationStepImplementation
import java.util.List

class TclValidator extends AbstractTclValidator {

	public static val UNKNOWN_NAME = 'unknownName'
	public static val NO_VALID_IMPLEMENTATION = 'noValidImplementation'

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
		if (testCase.specification != null) {
			if (!testCase.specification.steps.matches(testCase.steps))
				error("Testcase doesn't implent the Specification", TclPackage.Literals.TEST_CASE__SPECIFICATION,
					NO_VALID_IMPLEMENTATION)
		}
	}

	def boolean matches(List<SpecificationStep> specSteps, List<SpecificationStepImplementation> specImplSteps) {
		if (specSteps.size != specImplSteps.size) {
			return false
		}
		return specImplSteps.map[contents.restoreString].containsAll(specSteps.map[contents.restoreString])
	}
}
