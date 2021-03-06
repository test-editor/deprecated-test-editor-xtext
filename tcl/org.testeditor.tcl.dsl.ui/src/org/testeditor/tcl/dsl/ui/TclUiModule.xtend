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
package org.testeditor.tcl.dsl.ui

import com.google.inject.Binder
import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor
import org.testeditor.aml.dsl.ui.contentassist.AmlProposalProvider
import org.testeditor.aml.dsl.ui.internal.DslActivator
import org.testeditor.tcl.dsl.ide.highlighting.TclSemanticHighlightingCalculator
import org.testeditor.tcl.dsl.jvmmodel.TclBuilderParticipant
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

	override bindIXtextBuilderParticipant() {
		return TclBuilderParticipant
	}

	def void configureAmlProposalProvider(Binder binder) {
		binder.bind(AmlProposalProvider).toProvider [
			val amlInjector = DslActivator.instance.getInjector(DslActivator.ORG_TESTEDITOR_AML_DSL_AML)
			return amlInjector.getInstance(AmlProposalProvider)
		]
	}

}
