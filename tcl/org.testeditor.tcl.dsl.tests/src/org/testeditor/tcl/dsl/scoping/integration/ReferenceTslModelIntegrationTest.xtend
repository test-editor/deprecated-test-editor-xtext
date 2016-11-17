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
package org.testeditor.tcl.dsl.scoping.integration

import javax.inject.Inject
import org.junit.Test
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest
import org.testeditor.tcl.util.TclModelUtil

class ReferenceTslModelIntegrationTest extends AbstractParserTest {
	
	@Inject extension TclModelUtil

	private def parseTslModel(String packageName) {
		val tsl = '''
			package «packageName»
			
			# DummySpec
			* First step
		'''
		val tslModel = parseTsl(tsl, 'DummySpec.tsl').assertNoSyntaxErrors
		return tslModel
	}

	@Test
	def void canReferenceTslModelInSamePackage() {
		// given
		val tslModel = parseTslModel('com.example')
		val tcl = '''
			package com.example
			
			# DummySpecTest
			implements DummySpec
		'''

		// when
		val tclModel = parseTcl(tcl)

		// then
		tclModel.test.specification.assertSame(tslModel.specification)
	}

	@Test
	def void canImportTslModel() {
		// given
		val tslModel = parseTslModel('some.other')
		val tcl = '''
			package com.example
			
			import some.other.*
			
			# DummySpecTest
			implements DummySpec
		'''

		// when
		val tclModel = parseTcl(tcl)

		// then
		tclModel.test.specification.assertSame(tslModel.specification)
	}

	@Test
	def void referenceSpecificationStep() {
		// given
		val tslModel = parseTslModel('com.example')
		val firstStep = tslModel.specification.steps.assertSingleElement
		val tcl = '''
			package com.example
			
			# DummySpecTest implements DummySpec
			
			* First step
		'''
		val tclModel = parseTcl(tcl)
		val firstStepImpl = tclModel.test.steps.assertSingleElement

		// when
		val specificationStep = firstStepImpl.specificationStep

		// then
		specificationStep.assertSame(firstStep)
	}

}
