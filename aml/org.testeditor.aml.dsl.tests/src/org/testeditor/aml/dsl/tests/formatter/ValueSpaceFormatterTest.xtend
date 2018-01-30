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

import org.junit.Test

class ValueSpaceFormatterTest extends AbstractFormatterTest {
	
	@Test
	def void formatStringLiterals() {
		assertFormatted[
			expectation = '''
				value-space Colors = #[ "Red", "Green", "Blue" ]
			'''
			toBeFormatted = '''
				value-space Colors = #[ "Red" ,"Green"   , "Blue"]
			'''
		]
	}
	
	@Test
	def void formatIntegerRange() {
		assertFormatted[
			expectation = '''
				value-space DayInMonth = 1 .. 31
			'''
			toBeFormatted = '''
				value-space   DayInMonth=1..31
			'''
		]
	}
	
}