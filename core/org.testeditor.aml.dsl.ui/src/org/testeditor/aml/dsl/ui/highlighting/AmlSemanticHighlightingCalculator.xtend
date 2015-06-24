package org.testeditor.aml.dsl.ui.highlighting

import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.ui.editor.syntaxcoloring.DefaultSemanticHighlightingCalculator
import org.eclipse.xtext.ui.editor.syntaxcoloring.IHighlightedPositionAcceptor
import org.testeditor.aml.model.AmlModel
import org.testeditor.aml.model.Template
import org.testeditor.aml.model.TemplateConstant
import org.testeditor.aml.model.TemplateVariable

import static org.testeditor.aml.dsl.ui.highlighting.AmlHighlightingConfiguration.*

import static extension org.eclipse.xtext.nodemodel.util.NodeModelUtils.*

/**
 * Calculates highlighting.
 */
class AmlSemanticHighlightingCalculator extends DefaultSemanticHighlightingCalculator {

	override protected doProvideHighlightingFor(XtextResource resource, IHighlightedPositionAcceptor acceptor) {
		super.doProvideHighlightingFor(resource, acceptor)
		val root = resource.parseResult?.rootASTElement
		
		// Use AST for semantic highlighting 
		if (root instanceof AmlModel) {
			// Provide highlighting for all available templates.
			for (interactionType : root.interactionTypes) {
				interactionType.template?.provideHighlightingFor(acceptor)
			}
		}
	}

	/**
	 * Calculate highlighting for {@link Template}.
	 */
	protected def provideHighlightingFor(Template template, IHighlightedPositionAcceptor acceptor) {
		val templateNode = template.node
		acceptor.addPosition(templateNode.offset, templateNode.length, TEMPLATE)
		val constants = template.contents.filter(TemplateConstant)
		val variables = template.contents.filter(TemplateVariable)
		constants.map[node].forEach[acceptor.addPosition(offset, length, TEMPLATE, STRING)]
		variables.map[node].forEach[acceptor.addPosition(offset, length, TEMPLATE_VARIABLE)]
	}

}