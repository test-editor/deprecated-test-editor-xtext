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
package org.testeditor.aml.dsl.tests.formatter

import javax.inject.Inject
import javax.inject.Provider
import org.eclipse.xtext.testing.formatter.FormatterTestHelper
import org.eclipse.xtext.testing.formatter.FormatterTestRequest
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1
import org.testeditor.aml.dsl.tests.AbstractAmlTest

abstract class AbstractFormatterTest extends AbstractAmlTest {
	
	@Inject
	Provider<FormatterTestRequest> formatterRequestProvider

	@Inject
	protected FormatterTestHelper formatterTester

	/**
	 * Uses the {@link FormatterTester} but adds a default package
	 * <pre>
	 * 	package com.example
	 * </pre>
	 * So we don't need to repeat ourselves again and again.
	 */
	protected def void assertFormatted(Procedure1<FormatterTestRequest> init) {
		val request = formatterRequestProvider.get
		init.apply(request)
		if (request.expectation !== null) {
			request.expectation = '''
				package com.example
				
				«request.expectation»
			'''
		}
		request.toBeFormatted = '''
			package com.example
			
			«request.toBeFormatted»
		'''
		formatterTester.assertFormatted(request)
	}
	
}