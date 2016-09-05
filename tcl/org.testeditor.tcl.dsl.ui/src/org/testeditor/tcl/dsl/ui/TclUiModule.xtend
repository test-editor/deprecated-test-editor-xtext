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
package org.testeditor.tcl.dsl.ui

import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor
import org.testeditor.tcl.dsl.ide.highlighting.TclSemanticHighlightingCalculator
import org.testeditor.tcl.dsl.ui.contentassist.TclTemplateProposalProvider
import org.testeditor.tcl.dsl.ui.highlighting.TclHighlightingConfiguration
import org.testeditor.tcl.dsl.ui.navigation.TclHyperLinkHelper
import org.testeditor.tcl.dsl.ui.refatoring.TclJvmModelRenameStrategy

/**
 * Use this class to register components to be used within the Eclipse IDE.
 */
@FinalFieldsConstructor
class TclUiModule extends AbstractTclUiModule {

	override bindIHighlightingConfiguration() {
		return TclHighlightingConfiguration
	}

	override bindIdeSemanticHighlightingCalculator() {
		return TclSemanticHighlightingCalculator
	}

	override bindIHyperlinkHelper() {
		return TclHyperLinkHelper
	}

	override bindITemplateProposalProvider() {
		return TclTemplateProposalProvider
	}
				
	override bindIRenameStrategy() {
		return TclJvmModelRenameStrategy
	}
						
}
