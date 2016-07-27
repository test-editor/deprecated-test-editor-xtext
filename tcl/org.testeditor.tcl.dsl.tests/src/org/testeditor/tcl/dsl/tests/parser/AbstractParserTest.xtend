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
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.ParserRule
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.parser.IParseResult
import org.eclipse.xtext.parser.IParser
import org.eclipse.xtext.resource.XtextResourceSet
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.services.TclGrammarAccess
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import org.testeditor.tcl.util.TclModelUtil

abstract class AbstractParserTest extends AbstractTclTest {

	@Inject protected ParseHelper<TclModel> parseHelper
	@Inject protected IParser iparser
	@Inject protected extension TclModelUtil modelUtil
	@Inject protected TclGrammarAccess grammarAccess

	protected def TclModel parse(CharSequence input) {
		return parseHelper.parse(input)
	}

	protected def <T> T parse(CharSequence input, ParserRule rule, Class<T> ruleClass) {
		return iparser.parse(rule, new StringReader(input.toString)).assertNoSyntaxErrors.getParsedRule(ruleClass)
	}

	protected def IParseResult partialParse(ParserRule rule, CharSequence input) {
		iparser.parse(rule, new StringReader(input.toString))
	}

	protected def <T> T getParsedRule(IParseResult parseResult, Class<T> rule) {
		return rule.cast(parseResult.rootASTElement)
	}

	/** 
	 * register the given model with the resource set (for cross linking)
	 */
	protected def <T extends EObject> T register(T model, XtextResourceSet resourceSet, String fileName) {
		val uri = URI.createURI(fileName)
		val newResource = resourceSet.createResource(uri)
		newResource.contents.add(model)
		return model
	}

}
