package org.testeditor.tcl.dsl.ui.highlighting

import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.ui.editor.syntaxcoloring.DefaultSemanticHighlightingCalculator
import org.eclipse.xtext.ui.editor.syntaxcoloring.IHighlightedPositionAcceptor
import org.testeditor.aml.model.Template
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.TclModel

import static org.testeditor.tcl.dsl.ui.highlighting.TclHighlightingConfiguration.*

import static extension org.eclipse.xtext.nodemodel.util.NodeModelUtils.*

class TclSemanticHighlightingCalculator extends DefaultSemanticHighlightingCalculator {

	override protected doProvideHighlightingFor(XtextResource resource, IHighlightedPositionAcceptor acceptor) {
		super.doProvideHighlightingFor(resource, acceptor)
		val root = resource.parseResult?.rootASTElement

		// Use AST for semantic highlighting 
		if (root instanceof TclModel) {
			// Provide highlighting for all component element references
			for (specificationStep : root.steps) {
				for (context : specificationStep.contexts) {
					for (testStep : context.steps) {
						testStep.contents.filter(StepContentElement).forEach[provideHighlightingFor(acceptor)]
					}
				}
			}
		}
	}

	/**
	 * Calculate highlighting for {@link Template}.
	 */
	protected def provideHighlightingFor(StepContentElement componentElementReference, IHighlightedPositionAcceptor acceptor) {
		val node = componentElementReference.node
		acceptor.addPosition(node.offset, node.length, COMPONENT_ELEMENT_REFERENCE)
	}

}