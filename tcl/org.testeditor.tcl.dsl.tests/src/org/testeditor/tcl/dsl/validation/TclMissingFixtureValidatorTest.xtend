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

import org.eclipse.xtext.common.types.JvmParameterizedTypeReference
import org.eclipse.xtext.common.types.JvmType
import org.eclipse.xtext.validation.ValidationMessageAcceptor
import org.junit.Before
import org.junit.Test
import org.mockito.ArgumentCaptor
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.aml.InteractionType
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest
import org.testeditor.tcl.util.TclModelUtil

import static org.mockito.Matchers.*

import static extension org.mockito.Mockito.*

class TclMissingFixtureValidatorTest extends AbstractParserTest {

	@InjectMocks TclValidator tclValidator // class under test
	@Mock TclModelUtil tclModelUtil // injected into class under test
	@Mock JvmParameterizedTypeReference typeReferenceMock
	@Mock ValidationMessageAcceptor messageAcceptor

	val message = ArgumentCaptor.forClass(String)

	@Before
	override void setUp() {
		super.setUp

		val jvmTypeMock = JvmType.mock
		val interactionTypeMock = InteractionType.mock(RETURNS_DEEP_STUBS)

		when(tclModelUtil.getInteraction(anyObject)).thenReturn(interactionTypeMock)
		when(interactionTypeMock.defaultMethod.typeReference).thenReturn(typeReferenceMock)
		when(typeReferenceMock.type).thenReturn(jvmTypeMock) // default is != null => fixture exists 
		val state = tclValidator.setMessageAcceptor(messageAcceptor)
		state.state // needs to be called in order for internal state to be initialized. this again is necessary to allow messages to be issued on the "currentObject" of the validation
	}

	@Test
	def void noInfoOnExistingFixture() {
		// given
		val tclFix = parse('''
			package pa
			# Test
			
			* first
			Component: some_fantasy_component
			- test step that maps
		''')

		// when
		tclValidator.checkFixtureMethodForExistence(tclFix.steps.head.contexts.head.steps.head)

		// then
		messageAcceptor.verify(never).acceptInfo(anyString, anyObject, anyObject, anyInt, anyString)
	}

	@Test
	def void infoOnMissingFixture() {
		// given
		val tclFix = parse('''
			package pa
			# Test
			
			* first
			Component: some_fantasy_component
			- test step that does not map
		''')
		val testStepThatDoesNotMap = tclFix.steps.head.contexts.head.steps.head
		when(typeReferenceMock.type).thenReturn(null)

		// when
		tclValidator.checkFixtureMethodForExistence(testStepThatDoesNotMap)

		// then
		messageAcceptor.verify.acceptInfo(message.capture, anyObject, anyObject, anyInt, anyString)
		assertMatches(message.value, ".*could not resolve fixture")
	}

}
