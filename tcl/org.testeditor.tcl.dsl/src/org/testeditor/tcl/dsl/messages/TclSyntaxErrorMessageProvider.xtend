package org.testeditor.tcl.dsl.messages

import org.antlr.runtime.MismatchedTokenException
import org.eclipse.xtext.nodemodel.SyntaxErrorMessage
import org.eclipse.xtext.parser.antlr.SyntaxErrorMessageProvider
import org.testeditor.tcl.TclModel

class TclSyntaxErrorMessageProvider extends SyntaxErrorMessageProvider {

	public static val String MISSING_TEST_DESCRIPTION = "missingTestDescription"

	/**
	 * Customized error message missing test description
	 */
	override getSyntaxErrorMessage(IParserErrorContext context) {
		val currentContext = context?.currentContext
		if ((context?.recognitionException instanceof MismatchedTokenException) &&
			((context?.recognitionException as MismatchedTokenException )?.expecting == -1) &&
			(currentContext instanceof TclModel) && (currentContext as TclModel)?.test?.steps?.empty) {
			return new SyntaxErrorMessage('''
				Insert a test description before the actual test context/steps.
				E.g. "* This test will check that the answer will be 42" 
			''', TclSyntaxErrorMessageProvider.MISSING_TEST_DESCRIPTION)
		}
		super.getSyntaxErrorMessage(context)
	}

}
