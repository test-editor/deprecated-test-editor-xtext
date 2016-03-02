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
package org.testeditor.aml.dsl.ui.highlighting

import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.RGB
import org.eclipse.xtext.ui.editor.syntaxcoloring.DefaultHighlightingConfiguration
import org.eclipse.xtext.ui.editor.syntaxcoloring.IHighlightingConfigurationAcceptor
import org.eclipse.xtext.ui.editor.utils.TextStyle

/**
 * Defines default highlighting styles that are applied in case the user
 * did not overwrite them in the IDE.
 */
class AmlHighlightingConfiguration extends DefaultHighlightingConfiguration {

	public static val STRING = "string" // as defined in Xtext
	public static val TEMPLATE = "aml.template"
	public static val TEMPLATE_VARIABLE = "aml.template.variable"

	override configure(IHighlightingConfigurationAcceptor acceptor) {
		super.configure(acceptor)
		acceptor.acceptDefaultHighlighting(TEMPLATE, "Template", templateText)
		acceptor.acceptDefaultHighlighting(TEMPLATE_VARIABLE, "Template variable", templateVariable)
	}

	def TextStyle templateText() {
		return defaultTextStyle.copy => [
			backgroundColor = new RGB(240, 240, 240)
		]
	}

	def TextStyle templateVariable() {
		return defaultTextStyle.copy => [
			color = new RGB(50, 50, 50)
			style = SWT.ITALIC
		]
	}

}