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
package org.testeditor.tcl.dsl.messages

import org.antlr.runtime.MismatchedTokenException
import org.antlr.runtime.RecognitionException
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.nodemodel.SyntaxErrorMessage
import org.eclipse.xtext.parser.antlr.SyntaxErrorMessageProvider
import org.testeditor.tcl.TclModel

class TclSyntaxErrorMessageProvider extends SyntaxErrorMessageProvider {

	public static val String MISSING_TEST_DESCRIPTION = "missingTestDescription"

	/**
	 * Customized error message missing test description
	 */
	override getSyntaxErrorMessage(IParserErrorContext context) {
		if (context.recognitionException.isMismatchedTokenExceptionExpectingEOF &&
			context.currentContext.isTestModelWithoutTestStepsYet) {
			return new SyntaxErrorMessage('''
				Insert a test description before the actual test context.
				E.g. "* This test will check that the answer will be 42" 
			''', TclSyntaxErrorMessageProvider.MISSING_TEST_DESCRIPTION)
		}
		super.getSyntaxErrorMessage(context)
	}

	/**
	 * exception is a MismatchedTokenException raised because EOF is expected
	 */
	private def boolean isMismatchedTokenExceptionExpectingEOF(RecognitionException exception) {
		return (exception instanceof MismatchedTokenException) &&
			(exception as MismatchedTokenException).expecting == -1
	}

	/**
	 * context is a TclModel which has no steps defined yet
	 */
	private def boolean isTestModelWithoutTestStepsYet(EObject context) {
		if (context instanceof TclModel) {
			if (context.test !== null){
				return context.test.steps.empty
			}
		}
		return false
	}

}
