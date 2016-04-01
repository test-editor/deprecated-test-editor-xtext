package org.testeditor.tcl.dsl.messages

import javax.inject.Inject
import org.antlr.runtime.MismatchedTokenException
import org.antlr.runtime.RecognitionException
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.nodemodel.SyntaxErrorMessage
import org.eclipse.xtext.parser.antlr.SyntaxErrorMessageProvider
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.util.TclModelUtil

class TclSyntaxErrorMessageProvider extends SyntaxErrorMessageProvider {

	public static val String MISSING_TEST_DESCRIPTION = "missingTestDescription"

	/**
	 * Customized error message missing test description
	 */
	override getSyntaxErrorMessage(IParserErrorContext context) {
		if (context?.recognitionException.isAmismatchedTokenExceptionExpectingEOF &&
			context?.currentContext?.isAtestModelWithoutTestStepsYet) {
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
	private def boolean isAmismatchedTokenExceptionExpectingEOF(RecognitionException exception) {
		(exception instanceof MismatchedTokenException) && (exception as MismatchedTokenException )?.expecting == -1
	}

	/**
	 * context is a TclModel which has no steps defined yet
	 */
	private def boolean isAtestModelWithoutTestStepsYet(EObject context) {
		(context instanceof TclModel) && (context as TclModel)?.test?.steps?.empty
	}

}
