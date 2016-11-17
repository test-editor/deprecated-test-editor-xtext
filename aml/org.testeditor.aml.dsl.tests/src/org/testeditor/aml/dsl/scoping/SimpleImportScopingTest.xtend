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
package org.testeditor.aml.dsl.scoping

import javax.inject.Inject
import org.junit.Ignore
import org.junit.Test
import org.testeditor.dsl.common.testing.DslParseHelper

/**
 * Tests for importing AML elements.
 */
class SimpleImportScopingTest extends AbstractScopingTest {

	val file1 = '''
		package com.example
		
		component type Dialog
	'''
	
	@Inject extension DslParseHelper
	
	/**
	 * It should not be able to reference an element from a different 
	 * package without importing it.
	 */
	@Test
	def void testImportRequiredOnDifferentPackage() {
		// Given
		val file2 = '''
			package org.testeditor
			
			// no import here, should not be able to resolve Dialog
			
			component MyDialog is Dialog
		'''

		// When
		val model1 = parseAml(file1)
		val model2 = parseAml(file2)

		// Then
		model1.assertNoErrors
		val issue = model2.eResource.validate.assertSingleElement
		issue.message => [
			contains("cannot be resolved").assertTrue
			contains("Dialog").assertTrue
		]
	}

	/**
	 * It should be possible to reference an element from the same
	 * package but in a different file without importing it.
	 */
	@Test
	def void testNoImportRequiredOnSamePackage() {
		// Given
		val file2 = '''
			package com.example
			
			// no import should be required here
			
			component MyDialog is Dialog
		'''

		// When + Then
		parseAndVerifyLinking(file1, file2)
	}

	/**
	 * It should be possible to import an element from a different package 
	 * and reference it.
	 * 
	 * @Ignore since this is currently not possible since we don't generate
	 * JvmTypes. As soon as we do that this won't be an issue any longer.
	 * (XBases's XImportDeclaration requires a JvmDeclaredType for importedType). 
	 */
	@Test @Ignore
	def void testQualifiedImportWorks() {
		// Given
		val file2 = '''
			package org.testeditor
			
			import com.example.Dialog
			
			component MyDialog is Dialog
		'''

		// When + Then
		parseAndVerifyLinking(file1, file2)
	}
	
	/**
	 * It should be possible to reference an element from a different package
	 * using its fully qualified name.
	 */
	@Test
	def void testReferenceViaFullyQualifiedName() {
		// Given
		val file2 = '''
			package org.testeditor
			
			component MyDialog is com.example.Dialog
		'''
		
		// When + Then
		parseAndVerifyLinking(file1, file2)
	}

	/**
	 * It should be possible to use a wildcard import to a different package
	 * and reference those elements.
	 */
	@Test
	def void testWildcardImportWorks() {
		// Given
		val file2 = '''
			package org.testeditor
			
			import com.example.*
			
			component MyDialog is Dialog
		'''

		// When + Then
		parseAndVerifyLinking(file1, file2)
	}

	private def void parseAndVerifyLinking(String file1, String file2) {
		val model1 = parseAml(file1)
		val model2 = parseAml(file2)

		// Then
		model1.assertNoErrors
		model2.assertNoErrors
		val dialog = model1.componentTypes.assertSingleElement
		val myDialog = model2.components.assertSingleElement
		myDialog.type.assertSame(dialog)
	}

}
