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
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tsl.StepContent

import static org.testeditor.dsl.common.CommonPackage.Literals.*

import static extension org.eclipse.xtext.nodemodel.util.NodeModelUtils.getNode
import org.testeditor.tcl.TestStep

class TclSemanticHighlightingCalculator extends DefaultSemanticHighlightingCalculator {

	public static val String STEP_CONTENT_ELEMENT = "tsl.step_content"
	public static val String TEST_CASE_NAME = "testCase.name"
	public static val String MACRO_COLLECTION_NAME = "macroCollection.name"
	public static val String COMPONENT_ELEMENT_REFERENCE = "tcl.componentElementReference"

	@Inject extension NodeRegionUtil
	@Inject extension TclModelUtil

	override protected doProvideHighlightingFor(XtextResource resource, IHighlightedPositionAcceptor acceptor,
		CancelIndicator cancelIndicator) {
		super.doProvideHighlightingFor(resource, acceptor, cancelIndicator)
		if (cancelIndicator.canceled) {
			return
		}

		val root = resource.parseResult?.rootASTElement
		// Use AST for semantic highlighting 
		if (root instanceof TclModel) {
			root.doProvideHighlightingFor(acceptor, cancelIndicator)
		}
	}

	private def doProvideHighlightingFor(TclModel model, IHighlightedPositionAcceptor acceptor,
		CancelIndicator cancelIndicator) {
		// Provide highlighting for the name
		val nameRegion = model.findNodesRegionForFeature(NAMED_ELEMENT__NAME)
		if (nameRegion !== null) {
			if (model.test !== null) {
				acceptor.addPosition(nameRegion.offset, nameRegion.length, TEST_CASE_NAME)
			} else if (model.macroCollection !== null) {
				acceptor.addPosition(nameRegion.offset, nameRegion.length, MACRO_COLLECTION_NAME)
			}
		}

		// Provide highlighting for all component element references
		if (model.macroCollection !== null) {
			model.macroCollection.macros.map[contexts].flatten.forEach[provideHighlightingForTestStepContext(acceptor)]
		}
		if (model.test !== null) {
			model.test.doProvideHighlightingFor(acceptor, cancelIndicator)
		}
	}

	protected def dispatch void provideHighlightingForTestStepContext(ComponentTestStepContext context,
		IHighlightedPositionAcceptor acceptor) {
		context.steps.filter(TestStep).map[contents].flatten.forEach[provideHighlightingFor(acceptor)]
	}

	protected def dispatch void provideHighlightingForTestStepContext(MacroTestStepContext context,
		IHighlightedPositionAcceptor acceptor) {
		if (context.step !== null && context.step instanceof TestStep) {
			(context.step as TestStep).contents?.forEach[provideHighlightingFor(acceptor)]
		}
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

	private def doProvideHighlightingFor(TestCase test, IHighlightedPositionAcceptor acceptor,
		CancelIndicator cancelIndicator) {
		// Provide highlighting for the name
		// Provide highlighting for all component element references
		for (specificationStep : test.steps) {
			specificationStep.contents.forEach[provideHighlightingFor(acceptor)]
			val testSteps = specificationStep.contexts.map[testSteps].flatten
			val stepContents = testSteps.filter(TestStep).map[contents]
			stepContents.filter(StepContentElement).forEach [
				if (cancelIndicator.canceled) {
					return
				}
				provideHighlightingFor(acceptor)
			]
		}
	}

}
