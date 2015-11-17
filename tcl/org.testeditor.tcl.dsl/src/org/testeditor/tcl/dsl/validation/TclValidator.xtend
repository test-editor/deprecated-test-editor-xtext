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

import org.eclipse.xtext.xtype.XImportSection
import org.eclipse.xtext.validation.Check
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.TclPackage

class TclValidator extends AbstractTclValidator {

	public static val INVALID_NAME = 'invalidName'

	override checkImports(XImportSection importSection) {
		// ignore for now
	}

	@Check
	def checkMaskPresent(TestStepContext tsContext) {
			println(">>>>>>>>>> Hey " + tsContext.component.eIsProxy)
		if (tsContext.component.eIsProxy) {
			warning("mask is not defined in aml", TclPackage.Literals.TEST_STEP_CONTEXT__COMPONENT);
		}
	}

}
