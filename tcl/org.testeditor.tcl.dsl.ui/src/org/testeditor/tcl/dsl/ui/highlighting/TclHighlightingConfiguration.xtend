package org.testeditor.tcl.dsl.ui.highlighting

import org.eclipse.xtext.ui.editor.syntaxcoloring.DefaultHighlightingConfiguration
import org.eclipse.xtext.ui.editor.syntaxcoloring.IHighlightingConfigurationAcceptor
import org.eclipse.xtext.ui.editor.utils.TextStyle

class TclHighlightingConfiguration extends DefaultHighlightingConfiguration {

	public static val COMPONENT_ELEMENT_REFERENCE = "tcl.componentElementReference"

	override configure(IHighlightingConfigurationAcceptor acceptor) {
		super.configure(acceptor)
		acceptor.acceptDefaultHighlighting(COMPONENT_ELEMENT_REFERENCE, "Component Element Reference", componentElementReference)
	}

	def TextStyle componentElementReference() {
		return commentTextStyle.copy
	}

}