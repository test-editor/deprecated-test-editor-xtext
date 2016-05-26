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

import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.Assignment
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalAcceptor

class TclProposalProvider extends AbstractTclProposalProvider {

	override completeTestCase_Steps(EObject model, Assignment assignment, ContentAssistContext context,
		ICompletionProposalAcceptor acceptor) {
		// a proposal of missing steps is not trivial, since the order of the existing/missing steps must
		// be taken into account. the completion of steps should be done via (quick) fix.
		// this completion proposal will allow for steps to be entered even though no "implements" is given yet.
		acceptor.accept(createCompletionProposal('* ', '* test description', null, context))
	}

}
