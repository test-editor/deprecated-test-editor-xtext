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
package org.testeditor.dsl.common.testing

import javax.inject.Singleton
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.resource.XtextResource
import org.junit.Assert

@Singleton
class XtextAssertionHelper {

	def <T extends EObject> T assertNoSyntaxErrors(T element) {
		element.eResource.assertNoSyntaxErrors
		return element
	}

	def XtextResource assertNoSyntaxErrors(Resource resource) {
		val xtextResource = resource as XtextResource
		val errors = xtextResource.parseResult.syntaxErrors
		if (!errors.empty) {
			val message = '''
				Expected no syntax errors but got:
				«FOR error : errors»
					line «error.startLine»: «error.syntaxErrorMessage»
				«ENDFOR»
			'''
			Assert.fail(message)
		}
		return xtextResource
	}

}
