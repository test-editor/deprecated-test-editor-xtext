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
package org.testeditor.aml.dsl.tests.parser

import javax.inject.Inject
import org.junit.Test
import org.testeditor.aml.IntegerRange
import org.testeditor.aml.RegExValueSpace
import org.testeditor.aml.StringLiterals
import org.testeditor.dsl.common.testing.ResourceSetHelper

class ValueSpaceParserTest extends AbstractParserTest {

	@Inject extension ResourceSetHelper
	
	@Test
	def void parseStringLiterals() {
		// Given
		val stringLiterals = '''
			value-space Colors = #[ "Red", "Green", "Blue" ]
		'''

		// When + Then
		stringLiterals.parse(StringLiterals, resourceSet) => [
			assertNoErrors
			values.assertSize(3)
			values.get(0).assertEquals("Red")
			values.get(1).assertEquals("Green")
			values.get(2).assertEquals("Blue")
		]
	}

	@Test
	def void parseIntegerRange() {
		// Given
		val input = '''
			value-space DayInMonth = 1 .. 31
		'''

		// When + Then
		input.parse(IntegerRange, resourceSet) => [
			assertNoErrors
			from.assertEquals(1)
			to.assertEquals(31)
		]
	}

	@Test
	def void parseRegEx() {
		// Given
		val input = '''
			value-space RegEx = ".*"
		'''

		// When + Then
		input.parse(RegExValueSpace, resourceSet) => [
			expression.assertEquals('.*')
		]
	}

}
