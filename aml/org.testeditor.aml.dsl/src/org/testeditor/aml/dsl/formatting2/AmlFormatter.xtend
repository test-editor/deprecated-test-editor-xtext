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
package org.testeditor.aml.dsl.formatting2;

import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.formatting2.IFormattableDocument
import org.eclipse.xtext.xbase.formatting2.XbaseFormatter
import org.testeditor.aml.model.AmlModel
import org.testeditor.aml.model.Component
import org.testeditor.aml.model.IntegerRange
import org.testeditor.aml.model.StringLiterals

// import static org.testeditor.aml.model.ModelPackage.Literals.*

/**
 * Defines the formatting (pretty-print) for AML.
 */
class AmlFormatter extends XbaseFormatter {
	
	// TODO work in progress
	
	def dispatch void format(AmlModel model, extension IFormattableDocument document) {
		model.eContents.forEach[
			prepend[setNewLines(2, 2, 3)] // at least one empty line between elements
			formatBrackets(document)
			formatKeywords(document)
			format(document)
		]
	}
	
	def dispatch void format(Component component, extension IFormattableDocument document) {
		component.regionForKeyword("abstract").append[oneSpace]
		component.regionForKeyword("includes").append[oneSpace].prepend[oneSpace]
		component.elements.forEach[
			formatBrackets(document)
		]
	}
	
	def dispatch void format(StringLiterals element, extension IFormattableDocument document) {
		element.regionsForKeywords(",").forEach[prepend[noSpace].append[oneSpace]]
		element.regionForKeyword("#[").surround[oneSpace]		
		element.regionForKeyword("]").prepend[oneSpace]
	}
	
	def dispatch void format(IntegerRange element, extension IFormattableDocument document) {
		element.regionForKeyword("..").surround[oneSpace]
	}
	
	private def void formatBrackets(EObject element, extension IFormattableDocument document) {
		element.regionForKeyword("{").prepend[oneSpace].append[newLine; increaseIndentation]
		element.regionForKeyword("}").prepend[newLine; decreaseIndentation]
	}
	
	private def void formatKeywords(EObject element, extension IFormattableDocument document) {
		element.regionsForKeywords("component", "interaction", "element", "value-space").forEach[append[oneSpace]]
		element.regionForKeyword("type").surround[oneSpace]
		element.regionForKeyword("is").surround[oneSpace]
		element.regionForKeyword("=").surround[oneSpace]
		element.regionsForKeywords("label", "template").forEach[prepend[newLine]]
	}
	
}
