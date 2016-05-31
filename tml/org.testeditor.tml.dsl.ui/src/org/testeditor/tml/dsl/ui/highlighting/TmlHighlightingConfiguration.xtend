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
package org.testeditor.tml.dsl.ui.highlighting

import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.FontData
import org.eclipse.swt.graphics.RGB
import org.eclipse.xtext.ui.editor.syntaxcoloring.DefaultHighlightingConfiguration
import org.eclipse.xtext.ui.editor.syntaxcoloring.IHighlightingConfigurationAcceptor
import org.eclipse.xtext.ui.editor.utils.TextStyle

import static org.testeditor.tml.dsl.ide.highlighting.TmlSemanticHighlightingCalculator.*

class TmlHighlightingConfiguration extends DefaultHighlightingConfiguration {

	override configure(IHighlightingConfigurationAcceptor acceptor) {
		super.configure(acceptor)
		acceptor.acceptDefaultHighlighting(MACRO_NAME, "TmlModel Name", macroName)
		acceptor.acceptDefaultHighlighting(COMPONENT_ELEMENT_REFERENCE, "Component Element Reference",
			componentElementReference)
	}

	def TextStyle macroName() {
		return defaultTextStyle.copy => [
			color = new RGB(87, 195, 192)
			fontData = #[new FontData('Arial', 18, SWT.BOLD)]
		]
	}

	def TextStyle componentElementReference() {
		return commentTextStyle.copy
	}

}