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
package org.testeditor.tcl.dsl.formatting2;

import org.eclipse.xtext.formatting2.IFormattableDocument
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase
import org.testeditor.tml.dsl.formatting2.TmlFormatter

import static org.testeditor.tcl.TclPackage.Literals.*

class TclFormatter extends TmlFormatter {

	def dispatch void format(TclModel tclModel, extension IFormattableDocument document) {
		tclModel.regionFor.feature(TCL_MODEL__PACKAGE).append[newLines = 2]
		// import section is not formatted (yet), should be done by organize imports
		tclModel.regionFor.keyword("require").prepend[newLines = 2]
		tclModel.regionFor.keyword(",").prepend[noSpace]
		tclModel.environmentVariableReferences.forEach[prepend[oneSpace]]
		tclModel.test.format
	}

	def dispatch void format(TestCase testCase, extension IFormattableDocument document) {
		testCase.regionFor.keyword("#").prepend[newLines = 2].append[oneSpace]
		testCase.regionFor.feature(TEST_CASE__NAME).append[newLines = 2]
		// testCase.interior[indent] // configurable?
		testCase.steps.forEach[format]
	}

	def dispatch void format(SpecificationStepImplementation step, extension IFormattableDocument document) {
		step.regionFor.keyword("*").prepend[newLines = 2]
		step.contents.forEach[format]
		step.regionFor.keyword(".").prepend[noSpace]

		step.regionFor.feature(SPECIFICATION_STEP_IMPLEMENTATION__TEST).append[newLine]
		// step.interior[indent] // configurable
		step.contexts.forEach[format]
	}

}
