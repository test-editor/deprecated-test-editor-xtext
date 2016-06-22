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
package org.testeditor.tml.dsl.ide.highlighting

import javax.inject.Inject
import org.eclipse.xtext.ide.editor.syntaxcoloring.DefaultSemanticHighlightingCalculator
import org.eclipse.xtext.ide.editor.syntaxcoloring.IHighlightedPositionAcceptor
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.util.CancelIndicator
import org.testeditor.dsl.common.ide.util.NodeRegionUtil
import org.testeditor.tml.StepContentElement
import org.testeditor.tml.TmlModel
import org.testeditor.tsl.StepContent

import static org.testeditor.tml.TmlPackage.Literals.*

import static extension org.eclipse.xtext.nodemodel.util.NodeModelUtils.getNode
import org.testeditor.tml.ComponentTestStepContext
import org.testeditor.tml.MacroTestStepContext

class TmlSemanticHighlightingCalculator extends DefaultSemanticHighlightingCalculator {

	public static val String MACRO_NAME = "tml.name"
	public static val String COMPONENT_ELEMENT_REFERENCE = "tml.componentElementReference"
	public static val String STEP_CONTENT_ELEMENT = "tsl.step_content"

	@Inject extension NodeRegionUtil

	override protected doProvideHighlightingFor(XtextResource resource, IHighlightedPositionAcceptor acceptor,
		CancelIndicator cancelIndicator) {
		super.doProvideHighlightingFor(resource, acceptor, cancelIndicator)
		if (cancelIndicator.canceled) {
			return
		}

		val root = resource.parseResult?.rootASTElement
		// Use AST for semantic highlighting
		if (root instanceof TmlModel) {
			root.doProvideHighlightingFor(acceptor, cancelIndicator)
		}
	}

	private def doProvideHighlightingFor(TmlModel model, IHighlightedPositionAcceptor acceptor,
		CancelIndicator cancelIndicator) {
		// Provide highlighting for the name
		val region = model.findNodesRegionForFeature(MACRO_COLLECTION__NAME)
		if (region !== null) {
			acceptor.addPosition(region.offset, region.length, MACRO_NAME)
		}

		// Provide highlighting for all component element references
		model.macroCollection.macros.map[contexts].flatten.forEach [provideHighlightingForTestStepContext(acceptor)]
	}

	protected def dispatch void provideHighlightingForTestStepContext(ComponentTestStepContext context, IHighlightedPositionAcceptor acceptor){
		context.steps.map[contents].flatten.forEach[provideHighlightingFor(acceptor)]
	}

	protected def dispatch void provideHighlightingForTestStepContext(MacroTestStepContext context, IHighlightedPositionAcceptor acceptor){
		context.step.contents.forEach[provideHighlightingFor(acceptor)]
	}
	/**
	 * Calculate highlighting for {@link StepContentElement}.
	 */
	protected def void provideHighlightingFor(StepContentElement componentElementReference,
		IHighlightedPositionAcceptor acceptor) {
		val node = componentElementReference.node
		acceptor.addPosition(node.offset, node.length, COMPONENT_ELEMENT_REFERENCE)
	}

	/**
	 * Calculate highlighting for {@link StepContent}.
	 */
	protected def void provideHighlightingFor(StepContent componentElementReference,
		IHighlightedPositionAcceptor acceptor) {
		val node = componentElementReference.node
		acceptor.addPosition(node.offset, node.length, STEP_CONTENT_ELEMENT)
	}

}
