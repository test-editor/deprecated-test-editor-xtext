package org.testeditor.tcl.dsl.ui.navigation

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.ui.editor.hyperlinking.IHyperlinkAcceptor
import org.eclipse.xtext.xbase.ui.navigation.XbaseHyperLinkHelper
import org.testeditor.aml.model.ComponentElement
import org.testeditor.tcl.StepContent
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.util.TclModelUtil

class TclHyperLinkHelper extends XbaseHyperLinkHelper {

	@Inject extension TclModelUtil

	override createHyperlinksByOffset(XtextResource resource, int offset, IHyperlinkAcceptor acceptor) {
		super.createHyperlinksByOffset(resource, offset, acceptor)
		val element = getEObjectAtOffsetHelper.resolveElementAt(resource, offset)
		element.createHyperlinks(acceptor)
	}

	/**
	 * Create a hyperlink from a {@link TestStep} to its interaction's template. 
	 */
	protected def dispatch void createHyperlinks(TestStep testStep, IHyperlinkAcceptor acceptor) {
		val interaction = testStep.interaction
		if (interaction !== null) {
			testStep.createHyperlinkTo(interaction.template, acceptor)
		}
	}

	/**
	 * Create a hyperlink from {@link StepContentElement} to its referenced {@link ComponentElement}.
	 * 
	 * If it does not reference one, try to create a hyperlink for the eContainer at least.
	 */
	protected def dispatch void createHyperlinks(StepContentElement element, IHyperlinkAcceptor acceptor) {
		val componentElement = element.componentElement
		if (componentElement !== null) {
			element.createHyperlinkTo(componentElement, acceptor)
		} else {
			element.eContainer.createHyperlinks(acceptor)
		}
	}

	/**
	 * Create a hyperlink for the eContainer.
	 */
	protected def dispatch void createHyperlinks(StepContent content, IHyperlinkAcceptor acceptor) {
		content.eContainer.createHyperlinks(acceptor)
	}

	/** For all other types, do nothing. */
	protected def dispatch void createHyperlinks(EObject eObject, IHyperlinkAcceptor acceptor) {
		// do nothing
	}

	private def void createHyperlinkTo(EObject source, EObject target, IHyperlinkAcceptor acceptor) {
		val node = NodeModelUtils.findActualNodeFor(source)
		val resource = source.eResource as XtextResource
		createHyperlinksTo(resource, node, target, acceptor)
	}

}