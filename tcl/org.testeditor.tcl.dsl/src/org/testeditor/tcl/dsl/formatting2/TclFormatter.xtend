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
import org.testeditor.aml.Template
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.Macro
import org.testeditor.tcl.MacroCollection
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.StepContentVariableReference
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.TestCleanup
import org.testeditor.tcl.TestSetup
import org.testeditor.tcl.TestStep
import org.testeditor.tsl.StepContentText
import org.testeditor.tsl.StepContentVariable
import org.testeditor.tsl.TslPackage

import static org.eclipse.xtext.formatting2.IHiddenRegionFormatter.LOW_PRIORITY
import static org.testeditor.tcl.TclPackage.Literals.*

class TclFormatter extends XbaseFormatter {

	def dispatch void format(TclModel tclModel, extension IFormattableDocument document) {
		tclModel.regionFor.feature(TCL_MODEL__PACKAGE).append[newLines = 2]
		// import section is not formatted (yet), should be done by organize imports
		tclModel.regionFor.keyword("require").prepend[newLines = 2]
		tclModel.regionFor.keyword(",").prepend[noSpace]
		tclModel.environmentVariableReferences.forEach[prepend[oneSpace]]
		tclModel.test?.format
		tclModel.macroCollection?.format
	}

	def dispatch void format(TestCase testCase, extension IFormattableDocument document) {
		testCase.regionFor.keyword("#").prepend[newLines = 2].append[oneSpace]
		testCase.regionFor.keyword("implements").prepend[oneSpace]
		testCase.regionFor.feature(TEST_CASE__SPECIFICATION).prepend[oneSpace]
		// testCase.interior[indent] // configurable?
		testCase.setup?.format
		testCase.cleanup?.format
		testCase.steps.forEach[format]
	}

	def dispatch void format(TestSetup element, extension IFormattableDocument document) {
		element.regionFor.keyword("Setup").prepend[newLines = 2]
		element.regionFor.keyword(":").prepend[noSpace]
		element.interior[indent]
		element.contexts.forEach[format]
	}

	def dispatch void format(TestCleanup element, extension IFormattableDocument document) {
		element.regionFor.keyword("Cleanup").prepend[newLines = 2]
		element.regionFor.keyword(":").prepend[noSpace]
		element.interior[indent]
		element.contexts.forEach[format]
	}

	def dispatch void format(SpecificationStepImplementation step, extension IFormattableDocument document) {
		step.regionFor.keyword("*").prepend[newLines = 2]
		step.contents.forEach[format]
		step.regionFor.keyword(".").prepend[noSpace]

		step.regionFor.feature(SPECIFICATION_STEP_IMPLEMENTATION__TEST).append[newLine]
		step.interior[indent] // configurable?
		step.contexts.forEach[format]
	}

	def dispatch void format(MacroCollection macroCollection, extension IFormattableDocument document) {
		macroCollection.regionFor.keyword("#").prepend[newLines = 2].append[oneSpace]
		macroCollection.macros.forEach[format]
	}

	def dispatch void format(Macro macro, extension IFormattableDocument document) {
		macro.regionFor.keyword("##").prepend[newLines = 2].append[oneSpace]
		macro.regionFor.feature(MACRO__NAME).append[newLine]
		macro.regionFor.keyword("template").prepend[noIndentation].append[oneSpace]
		macro.regionFor.keyword("=").append[oneSpace]
		macro.template.format
		macro.interior[indent]
		macro.contexts.forEach[format]
	}

	def dispatch void format(ComponentTestStepContext componentTestStepContext,
		extension IFormattableDocument document) {
		componentTestStepContext.regionFor.keywords("Component", "Mask").forEach[prepend[newLines = 2].append[noSpace]]
		componentTestStepContext.regionFor.feature(COMPONENT_TEST_STEP_CONTEXT__COMPONENT) //
		.prepend[oneSpace] //
		.append[newLine; priority = LOW_PRIORITY]
		// componentTestStepContext.interior[indent] // configurable?
		componentTestStepContext.steps.forEach[format]
	}

	def dispatch void format(MacroTestStepContext macroTestStepContext, extension IFormattableDocument document) {
		macroTestStepContext.regionFor.keyword("Macro").prepend[newLines = 2].append[noSpace]
		macroTestStepContext.regionFor.feature(MACRO_TEST_STEP_CONTEXT__MACRO_COLLECTION) //
		.prepend[oneSpace] //
		.append[newLine; priority = LOW_PRIORITY]
		// macroTestStepContext.interior[indent] // configurable?
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
		stepContentElement.regionFor.keywords("<", "<>").forEach[prepend[oneSpace]]
	}

	def dispatch void format(StepContentVariableReference stepContentVariableReference,
		extension IFormattableDocument document) {
		stepContentVariableReference.regionFor.keyword('@').prepend[oneSpace]
		stepContentVariableReference.regionFor.feature(STEP_CONTENT_VARIABLE_REFERENCE__VARIABLE).prepend[noSpace]
	}

	def dispatch void format(Template template, extension IFormattableDocument document) {
		template.contents.forEach[it.prepend[oneSpace]]
	}

}
