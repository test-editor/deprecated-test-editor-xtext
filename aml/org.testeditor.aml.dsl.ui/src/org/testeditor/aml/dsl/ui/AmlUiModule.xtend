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
package org.testeditor.aml.dsl.ui

import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor
import org.testeditor.aml.dsl.ui.contentassist.AmlTemplateProposalProvider
import org.testeditor.aml.dsl.ui.highlighting.AmlHighlightingConfiguration

/**
 * Use this class to register components to be used within the Eclipse IDE.
 */
@FinalFieldsConstructor
class AmlUiModule extends AbstractAmlUiModule {

	override bindIHighlightingConfiguration() {
		return AmlHighlightingConfiguration
	}

	override bindITemplateProposalProvider() {
		return AmlTemplateProposalProvider
	}

}
