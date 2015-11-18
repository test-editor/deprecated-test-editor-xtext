/*******************************************************************************
 * Copyright (c) 2012 - 2015 Signal Iduna Corporation and others.
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
package org.testeditor.tcl.dsl.ui.quickfix

import org.eclipse.xtext.ui.editor.quickfix.Fix
import org.testeditor.tcl.dsl.validation.TclValidator

import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor
import org.eclipse.xtext.validation.Issue
import org.eclipse.xtext.ui.editor.model.edit.ISemanticModification
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext
import org.testeditor.tcl.TestStepContext

/**
 * Custom quickfixes.
 * 
 * See https://www.eclipse.org/Xtext/documentation/304_ide_concepts.html#quick-fixes
 */
class TclQuickfixProvider extends org.eclipse.xtext.xbase.ui.quickfix.XbaseQuickfixProvider {

	@Fix(TclValidator::UNKNOWN_NAME)
	def createAMLMask(Issue issue, IssueResolutionAcceptor acceptor) {
		acceptor.accept(issue, "Create AML Mask", "Creates a new AML Mask", 'upcase.png') [ element, context |
			if (element instanceof TestStepContext) {
				println(element.component)
			}
		]
	}
	
}

