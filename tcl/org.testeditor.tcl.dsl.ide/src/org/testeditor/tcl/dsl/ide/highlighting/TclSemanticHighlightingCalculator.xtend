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
package org.testeditor.tcl.dsl.ide.highlighting

import javax.inject.Inject
import org.eclipse.xtext.ide.editor.syntaxcoloring.DefaultSemanticHighlightingCalculator
import org.eclipse.xtext.ide.editor.syntaxcoloring.IHighlightedPositionAcceptor
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.util.CancelIndicator
import org.testeditor.dsl.common.ide.util.NodeRegionUtil
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase
import org.testeditor.tsl.StepContent

import static org.testeditor.tcl.TclPackage.Literals.*

import static extension org.eclipse.xtext.nodemodel.util.NodeModelUtils.getNode

class TclSemanticHighlightingCalculator extends DefaultSemanticHighlightingCalculator {

	public static val TEST_CASE_NAME = "tcl.testname"
	public static val COMPONENT_ELEMENT_REFERENCE = "tcl.componentElementReference"
	public static val STEP_CONTENT_ELEMENT = "tsl.step_content"

	@Inject extension NodeRegionUtil

	override protected doProvideHighlightingFor(XtextResource resource, IHighlightedPositionAcceptor acceptor,
		CancelIndicator cancelIndicator) {
		super.doProvideHighlightingFor(resource, acceptor, cancelIndicator)
		if (cancelIndicator.canceled) {
			return
		}

		val root = resource.parseResult?.rootASTElement
		// Use AST for semantic highlighting 
		if (root instanceof TclModel) {
			if (root.test !== null) {
				root.test.doProvideHighlightingFor(acceptor, cancelIndicator)
			}
		}
	}

	private def doProvideHighlightingFor(TestCase test, IHighlightedPositionAcceptor acceptor,
		CancelIndicator cancelIndicator) {
		// Provide highlighting for the name
		val region = test.findNodesRegionForFeature(TEST_CASE__NAME)
		if (region !== null) {
			acceptor.addPosition(region.offset, region.length, TEST_CASE_NAME)
		}

		// Provide highlighting for all component element references
		for (specificationStep : test.steps) {
			specificationStep.contents.forEach[provideHighlightingFor(acceptor)]
			for (context : specificationStep.contexts) {
				for (testStep : context.steps) {
					if (cancelIndicator.canceled) {
						return
					}
					testStep.contents.filter(StepContentElement).forEach[provideHighlightingFor(acceptor)]
				}
			}
		}
	}

	/**
	 * Calculate highlighting for {@link StepContentElement}.
	 */
	protected def provideHighlightingFor(StepContentElement componentElementReference,
		IHighlightedPositionAcceptor acceptor) {
		val node = componentElementReference.node
		acceptor.addPosition(node.offset, node.length, COMPONENT_ELEMENT_REFERENCE)
	}

	/**
	 * Calculate highlighting for {@link StepContent}.
	 */
	protected def provideHighlightingFor(StepContent componentElementReference, IHighlightedPositionAcceptor acceptor) {
		val node = componentElementReference.node
		acceptor.addPosition(node.offset, node.length, STEP_CONTENT_ELEMENT)
	}

}
