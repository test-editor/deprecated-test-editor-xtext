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
package org.testeditor.tcl.util

import java.util.Set
import javax.inject.Singleton
import org.eclipse.emf.ecore.EObject
import org.testeditor.tcl.EnvParam
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase
import org.testeditor.tml.ComponentTestStepContext
import org.testeditor.tml.MacroTestStepContext
import org.testeditor.tml.TestStep
import org.testeditor.tml.util.TmlModelUtil
import org.testeditor.tsl.SpecificationStep

@Singleton
class TclModelUtil extends TmlModelUtil {

	def SpecificationStep getSpecificationStep(SpecificationStepImplementation stepImplementation) {
		val tslModel = stepImplementation.test.specification
		if (tslModel !== null) {
			return tslModel.steps.findFirst[matches(stepImplementation)]
		}
		return null
	}

	def Iterable<SpecificationStep> getMissingTestSteps(TestCase testCase) {
		val specSteps = testCase.specification?.steps
		val steps = testCase.steps
		if (specSteps == null) {
			return emptyList
		}
		if (steps == null) {
			return specSteps.toList
		}
		return specSteps.filter [
			val specStepContentsString = contents.restoreString
			return steps.forall[contents.restoreString != specStepContentsString]
		]
	}

	def dispatch Iterable<TestStep> getTestSteps(ComponentTestStepContext context) {
		return context.steps
	}

	def dispatch Iterable<TestStep> getTestSteps(MacroTestStepContext context) {
		return #[context.step]
	}

	def Set<EnvParam> getEnvParams(EObject object) {
		var curObject = object
		while (curObject !== null) {
			if (curObject instanceof TclModel) {
				if (curObject.requiredEnvParams !== null) {
					return curObject.requiredEnvParams.envParams.toSet
				} else {
					return #{}
				}
			}
			curObject = curObject.eContainer
		}
		return #{}
	}

}
