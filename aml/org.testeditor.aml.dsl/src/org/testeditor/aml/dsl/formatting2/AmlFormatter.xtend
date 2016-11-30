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
package org.testeditor.aml.dsl.formatting2;

import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.formatting2.IFormattableDocument
import org.eclipse.xtext.xbase.formatting2.XbaseFormatter
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.IntegerRange
import org.testeditor.aml.StringLiterals
import org.testeditor.aml.ValueSpaceAssignment

 import static org.testeditor.aml.AmlPackage.Literals.*
 
/**
 * Defines the formatting (pretty-print) for AML.
 */
class AmlFormatter extends XbaseFormatter {

	def dispatch void format(AmlModel model, extension IFormattableDocument document) {
		model.eContents.forEach [
			prepend[setNewLines(2, 2, 2)] // at least one empty line between elements
			formatBrackets(document)
			formatKeywords(document)
		]
		model.eAllContents.forEach[format]
	}

	def dispatch void format(Component component, extension IFormattableDocument document) {
		component.regionFor.keyword("abstract").append[oneSpace]
		component.regionFor.keyword("includes").append[oneSpace].prepend[oneSpace]
	}

	def dispatch void format(StringLiterals element, extension IFormattableDocument document) {
		element.regionFor.keywords(",").forEach[prepend[noSpace].append[oneSpace]]
		element.regionFor.keyword("#[").surround[oneSpace]
		element.regionFor.keyword("]").prepend[oneSpace]
	}

	def dispatch void format(IntegerRange element, extension IFormattableDocument document) {
		element.regionFor.keyword("..").surround[oneSpace]
	}

	def dispatch void format(ComponentElement element, extension IFormattableDocument document) {
		element.interior[indent]
		element.formatBrackets(document, false)
	}

	def dispatch void format(ValueSpaceAssignment element, extension IFormattableDocument document) {
		element.prepend[setNewLines(1, 1, 2)] // <-- has to correspond to closing bracket in formatBrackets to match short/long syntax formatting
		element.regionFor.keyword('restrict').append[oneSpace]
		element.regionFor.keyword('to').surround[oneSpace]
		element.regionFor.feature(VALUE_SPACE_ASSIGNMENT__VARIABLE).append[oneSpace]
	}

	private def void formatBrackets(EObject element, extension IFormattableDocument document, boolean doIndent) {
		element.regionFor.keyword("{").prepend[oneSpace].append[setNewLines(1, 1, 2)]
		if (doIndent) {
			element.regionFor.keywordPairs("{", "}").forEach[interior[indent]]
		}
		element.regionFor.keyword("}").prepend[setNewLines(1, 1, 2)]
	}

	private def void formatBrackets(EObject element, extension IFormattableDocument document) {
		formatBrackets(element, document, true)
	}

	private def void formatKeywords(EObject element, extension IFormattableDocument document) {
		element.regionFor.keywords("component", "interaction", "element", "value-space").forEach[append[oneSpace]]
		element.regionFor.keyword("type").surround[oneSpace]
		element.regionFor.keyword("is").surround[oneSpace]
		element.regionFor.keyword("=").surround[oneSpace]
		element.regionFor.keywords("label", "template").forEach[prepend[setNewLines(1, 1, 2)]]
	}

}
