/*******************************************************************************
 * Copyright (c) 2012 - 2018 Signal Iduna Corporation and others.
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
package org.testeditor.aml.dsl.ui.highlighting

import org.eclipse.xtext.ide.editor.syntaxcoloring.DefaultSemanticHighlightingCalculator
import org.eclipse.xtext.ide.editor.syntaxcoloring.IHighlightedPositionAcceptor
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.util.CancelIndicator
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Template
import org.testeditor.aml.TemplateText
import org.testeditor.aml.TemplateVariable

import static org.testeditor.aml.dsl.ui.highlighting.AmlHighlightingConfiguration.*

import static extension org.eclipse.xtext.nodemodel.util.NodeModelUtils.*

/**
 * Calculates highlighting.
 */
class AmlSemanticHighlightingCalculator extends DefaultSemanticHighlightingCalculator {

	override protected doProvideHighlightingFor(XtextResource resource, IHighlightedPositionAcceptor acceptor, CancelIndicator cancelIndicator) {
		super.doProvideHighlightingFor(resource, acceptor, cancelIndicator)
		
		val root = resource.parseResult?.rootASTElement

		// Use AST for semantic highlighting 
		if (root instanceof AmlModel) {
			// Provide highlighting for all available templates.
			for (interactionType : root.interactionTypes) {
				if (cancelIndicator.canceled) {
					return
				}
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
		val constants = template.contents.filter(TemplateText)
		val variables = template.contents.filter(TemplateVariable)
		constants.map[node].forEach[acceptor.addPosition(offset, length, TEMPLATE, STRING)]
		variables.map[node].forEach[acceptor.addPosition(offset, length, TEMPLATE_VARIABLE)]
	}

}