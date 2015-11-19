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

import org.junit.Test

class SimpleTslParserTest extends AbstractParserTest {

	@Test
	def void parseEmptyModel() {
		'''
			package com.example
		'''.parse[
			assertNoErrors
			package.assertEquals('com.example')
		]
	}

	@Test
	def void parseSimpleSpecification() {
		'''
			package com.example
			
			* Start the famous greetings application.
		'''.parse[
			steps.assertSingleElement => [
				contents.map[value].join(' ').assertEquals('Start the famous greetings application')
			]
		]
	}
	
	@Test
	def void parseSpecificationWithVariable() {
		'''
			package com.example
			
			* Send greetings "Hello World" to the world.
		'''.parse[
			steps.assertSingleElement => [
				contents.map[value].join(' ').assertEquals('Send greetings Hello World to the world')
			]
		]
	}

}