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
package org.testeditor.tcl.dsl.tests.parser

import java.io.StringReader
import javax.inject.Inject
import org.apache.log4j.Logger
import org.eclipse.xtext.ParserRule
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.parser.IParseResult
import org.eclipse.xtext.parser.IParser
import org.junit.Assert
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.dsl.services.TclGrammarAccess
import org.testeditor.tcl.dsl.tests.AbstractTest
import org.testeditor.tcl.util.TclModelUtil

abstract class AbstractParserTest extends AbstractTest {

	private static val log = Logger.getLogger(AbstractParserTest)

	@Inject protected ParseHelper<TclModel> parser
	@Inject protected IParser iparser
	@Inject protected extension TclModelUtil modelUtil
	@Inject protected extension TclGrammarAccess

	protected def TestCase parse(CharSequence input) {
		return parser.parse(input).test
	}

	protected def <T> T parse(ParserRule rule, CharSequence input, Class<T> ruleClass, String ... strings) {
		return iparser.parse(rule, new StringReader(input.toString)).assertNoSyntaxErrors(strings).
			getParsedRule(ruleClass)
	}

	protected def IParseResult partialParse(ParserRule rule, CharSequence input) {
		iparser.parse(rule, new StringReader(input.toString))
	}

	protected def IParseResult assertNoSyntaxErrors(IParseResult parseResult, String ... strings) {
		if (parseResult.hasSyntaxErrors) {
			parseResult.syntaxErrors.forEach[log.info('''«syntaxErrorMessage»''')]
			Assert.fail(strings.join(System.lineSeparator))
		}
		return parseResult
	}

	protected def <T> T getParsedRule(IParseResult parseResult, Class<T> rule) {
		return rule.cast(parseResult.rootASTElement)
	}

}
