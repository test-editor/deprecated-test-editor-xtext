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
package org.testeditor.tcl.dsl.validation

import javax.inject.Inject
import org.eclipse.xtext.testing.validation.ValidationTestHelper
import org.junit.Test
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest

import static org.testeditor.tcl.TclPackage.Literals.*
import java.util.List

class TclSetupCleanupValidatorTest extends AbstractParserTest {

	@Inject
	ValidationTestHelper validator

	def TclModel tclWithSetupAndCleanupSections(List<Boolean> setupCleanupConfig) {
		var dummyComponent = '''
			Component: Some
			- some
		'''
		return '''
			package com.example
			
			# Test
			
			«IF (setupCleanupConfig.get(0))»
				Setup: 
				  «dummyComponent»
			«ENDIF»
			«IF (setupCleanupConfig.get(1))»
				Cleanup:
				  «dummyComponent»
			«ENDIF»
			* spec step
			  «dummyComponent»
			«IF (setupCleanupConfig.get(2))»
				Cleanup:
				  «dummyComponent»
			«ENDIF»
			«IF (setupCleanupConfig.get(3))»
				Setup:
				  «dummyComponent»
			«ENDIF»
		'''.toString.parseTcl
	}

	@Test
	def void validateSetupCleanupSectionCombination() {
		// given
		val setupCleanupSectionToExpectedErrorsMap = #{
			// [setupPre, cleanupPre, cleanupPost, setupPost] -> [setup error, cleanup error] 
			#[false, false, false, false] -> #[false, false],
			#[true, false, false, false] -> #[false, false],
			#[false, true, false, false] -> #[false, false],
			#[true, true, false, false] -> #[false, false],
			#[false, false, true, false] -> #[false, false],
			#[true, false, true, false] -> #[false, false],
			#[false, true, true, false] -> #[false, true],
			#[true, true, true, false] -> #[false, true],
			#[false, false, false, true] -> #[false, false],
			#[true, false, false, true] -> #[true, false],
			#[false, true, false, true] -> #[false, false],
			#[true, true, false, true] -> #[true, false],
			#[false, false, true, true] -> #[false, false],
			#[true, false, true, true] -> #[true, false],
			#[false, true, true, true] -> #[false, true],
			#[true, true, true, true] -> #[true, true]
		}

		setupCleanupSectionToExpectedErrorsMap.forEach [ setupCleanupConfig, expectedErrors |
			// when
			val model = tclWithSetupAndCleanupSections(setupCleanupConfig)

			// then
			try {
				val setupSectionError = expectedErrors.get(0)				
 				if (!setupSectionError) {
					validator.assertNoErrors(model, SETUP_AND_CLEANUP_PROVIDER, TclValidator.MULTIPLE_SETUP_SECTIONS)
				} else {
					validator.assertError(model, SETUP_AND_CLEANUP_PROVIDER, TclValidator.MULTIPLE_SETUP_SECTIONS)
				}
				
				val cleanupSectionError = expectedErrors.get(1)
				if (!cleanupSectionError) {
					validator.assertNoErrors(model, SETUP_AND_CLEANUP_PROVIDER, TclValidator.MULTIPLE_CLEANUP_SECTIONS)
				} else {
					validator.assertError(model, SETUP_AND_CLEANUP_PROVIDER, TclValidator.MULTIPLE_CLEANUP_SECTIONS)
				}
			} catch (AssertionError assertionError) {
				// in case of errors, this is very helpful!
				throw new AssertionError('''
					Setup-Cleanup combination check failed, check the following combination: 
					setupPre, cleanupPre, cleanupPost,setupPost: «setupCleanupConfig»
					expected errors: «expectedErrors»
				''', assertionError)
			}
		]

	}

}
