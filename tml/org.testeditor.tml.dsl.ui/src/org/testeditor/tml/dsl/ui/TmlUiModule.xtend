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
package org.testeditor.tml.dsl.ui

import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor
import org.testeditor.tml.dsl.ui.highlighting.TmlHighlightingConfiguration
import org.testeditor.tml.dsl.ide.highlighting.TmlSemanticHighlightingCalculator
import org.testeditor.tml.dsl.ui.navigation.TmlHyperLinkHelper
import org.testeditor.tml.dsl.ui.contentassist.TmlTemplateProposalProvider

/**
 * Use this class to register components to be used within the Eclipse IDE.
 */
@FinalFieldsConstructor
class TmlUiModule extends AbstractTmlUiModule {
	override bindIHighlightingConfiguration() {
		return TmlHighlightingConfiguration
	}

	override bindIdeSemanticHighlightingCalculator() {
		return TmlSemanticHighlightingCalculator
	}

	override bindIHyperlinkHelper() {
		return TmlHyperLinkHelper
	}

	override bindITemplateProposalProvider() {
		return TmlTemplateProposalProvider
	}
}
