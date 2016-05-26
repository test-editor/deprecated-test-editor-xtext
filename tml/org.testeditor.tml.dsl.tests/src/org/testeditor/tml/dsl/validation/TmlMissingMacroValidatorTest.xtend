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
package org.testeditor.tml.dsl.validation

import org.eclipse.xtext.validation.ValidationMessageAcceptor
import org.junit.Before
import org.junit.Test
import org.mockito.ArgumentCaptor
import org.mockito.Captor
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.aml.Template
import org.testeditor.tml.MacroTestStepContext
import org.testeditor.tml.dsl.tests.parser.AbstractParserTest
import org.testeditor.tml.util.TmlModelUtil

import static org.mockito.Matchers.*

import static extension org.mockito.Mockito.*
import org.testeditor.tml.TestStep

class TmlMissingMacroValidatorTest extends AbstractParserTest {

	@InjectMocks TmlValidator tmlValidator // class under test
	@Mock TmlModelUtil tmlModelUtil // injected into class under test
	@Mock ValidationMessageAcceptor messageAcceptor
	@Mock MacroTestStepContext macroTestStepContextMock

	@Captor ArgumentCaptor<String> message

	@Before
	def void initMocks() {
		when(tmlModelUtil.getMacroContext(anyObject)).thenReturn(macroTestStepContextMock)
		when(tmlModelUtil.hasComponentContext(anyObject)).thenReturn(false)
		when(tmlModelUtil.hasMacroContext(anyObject)).thenReturn(true)
		when(tmlModelUtil.normalize(any(TestStep))).thenReturn("abc")
		when(tmlModelUtil.normalize(any(Template))).thenReturn("abc")
		val state = tmlValidator.setMessageAcceptor(messageAcceptor)
		state.state // needs to be called in order for internal state to be initialized. this again is necessary to allow messages to be issued on the "currentObject" of the validation
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
		val testStepThatMaps = tmlModel.macros.head.contexts.head.assertInstanceOf(MacroTestStepContext).step
		when(macroTestStepContextMock.macroModel).thenReturn(tmlModel)

		// when
		tmlValidator.checkMacroCall(testStepThatMaps)

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
		val testStepThatDoesNotMap = tmlModel.macros.head.contexts.head.assertInstanceOf(MacroTestStepContext).step
		when(tmlModelUtil.normalize(any(Template))).thenReturn("cba")
		when(macroTestStepContextMock.macroModel).thenReturn(tmlModel)

		// when
		tmlValidator.checkMacroCall(testStepThatDoesNotMap)

		// then
		messageAcceptor.verify.acceptWarning(message.capture, anyObject, anyObject, anyInt, anyString)
		assertMatches(message.value, ".*could not resolve macro.*")
	}

}
