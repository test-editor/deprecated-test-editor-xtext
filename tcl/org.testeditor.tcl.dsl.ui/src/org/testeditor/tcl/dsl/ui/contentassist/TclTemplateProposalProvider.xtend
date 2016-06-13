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
package org.testeditor.tcl.dsl.ui.contentassist

import javax.inject.Inject
import org.eclipse.jface.text.templates.ContextTypeRegistry
import org.eclipse.jface.text.templates.TemplateContext
import org.eclipse.jface.text.templates.persistence.TemplateStore
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext
import org.eclipse.xtext.ui.editor.contentassist.ITemplateAcceptor
import org.eclipse.xtext.ui.editor.templates.ContextTypeIdHelper
import org.testeditor.tml.dsl.ui.contentassist.TmlTemplateProposalProvider

class TclTemplateProposalProvider extends TmlTemplateProposalProvider {

	@Inject
	new(TemplateStore templateStore, ContextTypeRegistry registry, ContextTypeIdHelper helper) {
		super(templateStore, registry, helper)
	}

	/**
	 * Customize template creation to support dynamic templates.
	 */
	override protected createTemplates(TemplateContext templateContext, ContentAssistContext context,
		ITemplateAcceptor acceptor) {
		super.createTemplates(templateContext, context, acceptor)

	}

}
