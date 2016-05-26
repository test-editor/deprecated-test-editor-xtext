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
package org.testeditor.tcl.dsl.ui.highlighting

import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.FontData
import org.eclipse.swt.graphics.RGB
import org.eclipse.xtext.ui.editor.syntaxcoloring.IHighlightingConfigurationAcceptor
import org.eclipse.xtext.ui.editor.utils.TextStyle
import org.testeditor.tml.dsl.ui.highlighting.TmlHighlightingConfiguration

import static org.testeditor.tcl.dsl.ide.highlighting.TclSemanticHighlightingCalculator.*

class TclHighlightingConfiguration extends TmlHighlightingConfiguration {

	override configure(IHighlightingConfigurationAcceptor acceptor) {
		super.configure(acceptor)
		acceptor.acceptDefaultHighlighting(TEST_CASE_NAME, "TestCase Name", testCaseName)
		acceptor.acceptDefaultHighlighting(STEP_CONTENT_ELEMENT, "Step content element", stepContentElement)
	}

	def TextStyle testCaseName() {
		return defaultTextStyle.copy => [
			color = new RGB(87, 195, 192)
			fontData = #[new FontData('Arial', 18, SWT.BOLD)]
		]
	}

	def TextStyle stepContentElement() {
		val textStyle = defaultTextStyle.copy
		textStyle.style = SWT.BOLD.bitwiseOr(SWT.ITALIC)
		return textStyle
	}

}
