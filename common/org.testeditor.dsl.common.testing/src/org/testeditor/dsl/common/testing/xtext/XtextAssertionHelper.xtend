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
package org.testeditor.dsl.common.testing.xtext

import java.util.ArrayList
import javax.inject.Singleton
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.parser.IParseResult
import org.eclipse.xtext.resource.XtextResource
import org.junit.Assert
import org.slf4j.LoggerFactory

@Singleton
class XtextAssertionHelper {

	static val logger = LoggerFactory.getLogger(XtextAssertionHelper)

	def <T extends EObject> T assertNoSyntaxErrors(T element) {
		element.eResource.assertNoSyntaxErrors
		return element
	}

	def XtextResource assertNoSyntaxErrors(Resource resource) {
		val xtextResource = resource as XtextResource
		xtextResource.parseResult.assertNoSyntaxErrors
		return xtextResource
	}

	def IParseResult assertNoSyntaxErrors(IParseResult parseResult) {
		val errors = parseResult.syntaxErrors
		if (!errors.empty) {
			val message = '''
				Expected no syntax errors but got:
				«FOR error : errors»
					line «error.startLine»: «error.syntaxErrorMessage»
				«ENDFOR»
			'''
			logger.error('''
				«message»
				
				Input:
				«parseResult.textWithLineNumbers»
			''')
			Assert.fail(message)
		}
		return parseResult
	}

	private def String getTextWithLineNumbers(IParseResult parseResult) {
		val text = parseResult.rootNode.text
		val split = text.split('\r?\n')
		val result = new ArrayList(split.size)
		split.forEach[line, number|result += '''«number+1» | «line»''']
		return result.join(System.lineSeparator)
	}

}
