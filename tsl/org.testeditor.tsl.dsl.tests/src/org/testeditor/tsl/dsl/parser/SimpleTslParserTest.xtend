/*******************************************************************************
 * Copyright (c) 2012 - 2015 Signal Iduna Corporation and others.
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
package org.testeditor.tsl.dsl.parser

import javax.inject.Inject
import org.junit.Test
import org.testeditor.tsl.util.TslModelUtil

class SimpleTslParserTest extends AbstractParserTest {

	@Inject extension TslModelUtil

	@Test
	def void parseEmptyModel() {
		// given
		val tsl = '''
			package com.example
		'''
		
		// expect
		tsl.parse [
			package.assertEquals('com.example')
			specification.assertNull
		]
	}

	@Test
	def void parseEmptySpecification() {
		// given
		val tsl = '''
			package com.example
			
			# Test
		'''
		
		// expect
		tsl.parse [
			specification.name.assertEquals('Test')
		]
	}

	@Test
	def void parseSpecificationWithVariable() {
		// given
		val tsl = '''
			package com.example
			
			# Test
			* Send greetings "Hello World" to the world.
		'''
		
		// expect
		tsl.parse [
			specification.steps.assertSingleElement => [
				contents.restoreString.assertEquals('Send greetings "Hello World" to the world')
			]
		]
	}

}