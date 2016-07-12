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
package org.testeditor.tcl.dsl.validation

import org.junit.Test
import org.mockito.ArgumentCaptor
import org.mockito.Captor
import org.mockito.Mock
import org.testeditor.aml.Template
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.TestStep

import static org.mockito.Matchers.*

import static extension org.mockito.Mockito.*
import org.junit.Before

class TclMissingMacroValidatorTest extends AbstractTclValidatorTest {

	@Mock MacroTestStepContext macroTestStepContextMock

	@Captor ArgumentCaptor<String> message

	@Before
	def void initMocks() {
		when(tclModelUtil.getMacroContext(anyObject)).thenReturn(macroTestStepContextMock)
		when(tclModelUtil.hasComponentContext(anyObject)).thenReturn(false)
		when(tclModelUtil.hasMacroContext(anyObject)).thenReturn(true)
		when(tclModelUtil.normalize(any(TestStep))).thenReturn("abc")
		when(tclModelUtil.normalize(any(Template))).thenReturn("abc")
	}

	@Test
	def void noInfoOnExistingMacro() {
		// given
		val tmlModel = parse('''
			package pa
			# MacroCollection
			
			## HelloMacro
			template = "hello"
			Macro: some_fantasy_macro
			- macro call that maps
		''')
		val testStepThatMaps = tmlModel.macroCollection.macros.head.contexts.head.assertInstanceOf(
			MacroTestStepContext).step
		when(macroTestStepContextMock.macroCollection).thenReturn(tmlModel.macroCollection)

		// when
		tclValidator.checkMacroCall(testStepThatMaps)

		// then
		messageAcceptor.verify(never).acceptInfo(anyString, anyObject, anyObject, anyInt, anyString)
	}

	@Test
	def void warningOnMissingMacro() {
		// given
		val tmlModel = parse('''
			package pa
			# MacroCollection
			
			## HelloMacro
			template = "hello"
			Macro: some_fantasy_macro
			- macro call that does not maps
		''')
		val testStepThatDoesNotMap = tmlModel.macroCollection.macros.head.contexts.head.assertInstanceOf(
			MacroTestStepContext).step
		when(tclModelUtil.normalize(any(Template))).thenReturn("cba")
		when(macroTestStepContextMock.macroCollection).thenReturn(tmlModel.macroCollection)

		// when
		tclValidator.checkMacroCall(testStepThatDoesNotMap)

		// then
		messageAcceptor.verify.acceptWarning(message.capture, anyObject, anyObject, anyInt, anyString)
		assertMatches(message.value, ".*could not resolve macro.*")
	}

}