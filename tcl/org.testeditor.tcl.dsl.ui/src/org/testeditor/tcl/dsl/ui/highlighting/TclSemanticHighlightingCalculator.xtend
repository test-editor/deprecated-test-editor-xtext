package org.testeditor.tcl.dsl.ui.highlighting

import java.util.List
import org.eclipse.jface.text.Region
import org.eclipse.xtext.nodemodel.ILeafNode
import org.eclipse.xtext.nodemodel.INode
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.ui.editor.syntaxcoloring.DefaultSemanticHighlightingCalculator
import org.eclipse.xtext.ui.editor.syntaxcoloring.IHighlightedPositionAcceptor
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TclPackage
import org.testeditor.tcl.TestCase

import static org.testeditor.tcl.dsl.ui.util.NodeRegionUtil.*

import static extension org.eclipse.xtext.nodemodel.util.NodeModelUtils.*

class TclSemanticHighlightingCalculator extends DefaultSemanticHighlightingCalculator {

	override protected doProvideHighlightingFor(XtextResource resource, IHighlightedPositionAcceptor acceptor) {
		super.doProvideHighlightingFor(resource, acceptor)
		val root = resource.parseResult?.rootASTElement

		// Use AST for semantic highlighting 
		if (root instanceof TclModel) {
			if (root.test !== null) {
				root.test.doProvideHighlightingFor(acceptor)
			}
		}
	}

	private def doProvideHighlightingFor(TestCase test, IHighlightedPositionAcceptor acceptor) {
		// Provide highlighting for the name
		val region = test.regionForName
		if (region !== null) {
			acceptor.addPosition(region.offset, region.length, TclHighlightingConfiguration.TEST_CASE_NAME)
		}

		// Provide highlighting for all component element references
		for (specificationStep : test.steps) {
			for (context : specificationStep.contexts) {
				for (testStep : context.steps) {
					testStep.contents.filter(StepContentElement).forEach[provideHighlightingFor(acceptor)]
				}
			}
		}
	}

	/**
	 * Calculate the region for
	 * <pre>
	 * # test.name
	 * </pre>
	 */
	private def Region getRegionForName(TestCase test) {
		val nameNodes = NodeModelUtils.findNodesForFeature(test, TclPackage.Literals.TEST_CASE__NAME)
		if (!nameNodes.empty) {
			val List<INode> nodes = newLinkedList()
			val previous = nameNodes.head.previousNonHiddenLeafNode
			if (previous !== null) {
				nodes += previous
			}
			nodes += nameNodes
			return getRegion(nodes)
		}
		return null
	}
	
	private def previousNonHiddenLeafNode(INode node) {
		var previous = node.previousSibling
		while (previous !== null) {
			if (previous instanceof ILeafNode) {
				if (!previous.hidden) {
					return previous
				}
				previous = previous.previousSibling
			} else {
				return null
			}
		}
	}
	

	/**
	 * Calculate highlighting for {@link StepContentElement}.
	 */
	protected def provideHighlightingFor(StepContentElement componentElementReference,
		IHighlightedPositionAcceptor acceptor) {
		val node = componentElementReference.node
		acceptor.addPosition(node.offset, node.length, TclHighlightingConfiguration.COMPONENT_ELEMENT_REFERENCE)
	}

}