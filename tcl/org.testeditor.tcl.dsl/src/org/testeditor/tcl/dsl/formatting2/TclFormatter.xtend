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
import org.eclipse.xtext.xbase.formatting2.XbaseFormatter
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TclPackage
import org.testeditor.tcl.TestCase
import org.testeditor.tml.ComponentTestStepContext
import org.testeditor.tml.MacroTestStepContext
import org.testeditor.tml.StepContentElement
import org.testeditor.tml.StepContentVariableReference
import org.testeditor.tml.TestStep
import org.testeditor.tml.TmlPackage
import org.testeditor.tsl.StepContentText
import org.testeditor.tsl.StepContentVariable
import org.testeditor.tsl.TslPackage

class TclFormatter extends XbaseFormatter {

	def dispatch void format(TclModel tclModel, extension IFormattableDocument document) {
		tclModel.regionFor.feature(TclPackage.Literals.TCL_MODEL__PACKAGE).append[newLines = 2]
		// import section is not formatted (yet), should be done by organize imports
		tclModel.regionFor.keyword("require").prepend[newLines = 2]
		tclModel.regionFor.keyword(",").prepend[noSpace]
		tclModel.environmentVariableReferences.forEach[prepend[oneSpace]]
		tclModel.test.format
	}

	def dispatch void format(TestCase testCase, extension IFormattableDocument document) {
		testCase.regionFor.keyword("#").prepend[newLines = 2].append[oneSpace]
		testCase.regionFor.feature(TclPackage.Literals.TEST_CASE__NAME).append[newLines = 2]
		// testCase.interior[indent] // configurable?
		testCase.steps.forEach[format]
	}

	def dispatch void format(SpecificationStepImplementation step, extension IFormattableDocument document) {
		step.regionFor.keyword("*").prepend[newLines = 2]
		step.contents.forEach[format]
		step.regionFor.keyword(".").prepend[noSpace]

		step.regionFor.feature(TclPackage.Literals.SPECIFICATION_STEP_IMPLEMENTATION__TEST).append[newLine]
		// step.interior[indent] // configurable
		step.contexts.forEach[format]
	}

	def dispatch void format(ComponentTestStepContext componentTestStepContext,
		extension IFormattableDocument document) {
		componentTestStepContext.regionFor.keyword("Component").prepend[newLine].append[noSpace]
		componentTestStepContext.regionFor.keyword("Mask").prepend[newLine].append[noSpace]
		componentTestStepContext.regionFor.feature(TmlPackage.Literals.COMPONENT_TEST_STEP_CONTEXT__COMPONENT).prepend [
			oneSpace
		].append[newLine]
		// componentTestStepContext.interior[indent] // configurable
		componentTestStepContext.steps.forEach[format]
	}

	def dispatch void format(MacroTestStepContext macroTestStepContext, extension IFormattableDocument document) {
		macroTestStepContext.regionFor.keyword("Macro").prepend[newLine].append[noSpace]
		macroTestStepContext.regionFor.feature(TmlPackage.Literals.MACRO_TEST_STEP_CONTEXT__MACRO_MODEL).prepend [
			oneSpace
		]
		macroTestStepContext.interior[indent]
		macroTestStepContext.step.format
	}

	def dispatch void format(TestStep testStep, extension IFormattableDocument document) {
		testStep.regionFor.keyword("-").prepend[newLine]
		testStep.contents.forEach[format]
		testStep.regionFor.keyword(".").prepend[noSpace]
	}

	def dispatch void format(StepContentText stepContentText, extension IFormattableDocument document) {
		stepContentText.regionFor.feature(TslPackage.Literals.STEP_CONTENT_VALUE__VALUE).prepend[oneSpace]
	}

	def dispatch void format(StepContentVariable stepContentVariable, extension IFormattableDocument document) {
		stepContentVariable.regionFor.feature(TslPackage.Literals.STEP_CONTENT_VALUE__VALUE).prepend[oneSpace]
	}

	def dispatch void format(StepContentElement stepContentElement, extension IFormattableDocument document) {
		stepContentElement.regionFor.keyword("<").prepend[oneSpace]
		stepContentElement.regionFor.keyword("<>").prepend[oneSpace]
	}

	def dispatch void format(StepContentVariableReference stepContentVariableReference,
		extension IFormattableDocument document) {
		stepContentVariableReference.regionFor.keyword('@').prepend[oneSpace]
		stepContentVariableReference.regionFor.feature(TmlPackage.Literals.STEP_CONTENT_VARIABLE_REFERENCE__VARIABLE).
			prepend[noSpace]
	}

}
