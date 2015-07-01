package org.testeditor.aml.dsl.formatting2;

import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.formatting2.IFormattableDocument
import org.eclipse.xtext.xbase.formatting2.XbaseFormatter
import org.testeditor.aml.model.AmlModel
import org.testeditor.aml.model.Component

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
	
	protected def void formatBrackets(EObject element, extension IFormattableDocument document) {
		element.regionForKeyword("{").prepend[oneSpace].append[newLine; increaseIndentation]
		element.regionForKeyword("}").prepend[newLine; decreaseIndentation]
	}
	
	protected def void formatKeywords(EObject element, extension IFormattableDocument document) {
		element.regionsForKeywords("component", "interaction", "element").forEach[append[oneSpace]]
		element.regionForKeyword("type").prepend[oneSpace].append[oneSpace]
		element.regionForKeyword("is").prepend[oneSpace].append[oneSpace]
		element.regionForKeyword("=").prepend[oneSpace].append[oneSpace]
		element.regionsForKeywords("label", "template").forEach[prepend[newLine]]
	}
	
}
