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

import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference
import org.eclipse.xtext.common.types.JvmType
import org.eclipse.xtext.validation.ValidationMessageAcceptor
import org.junit.Before
import org.junit.Test
import org.mockito.ArgumentCaptor
import org.mockito.InjectMocks
import org.mockito.Mock
import org.mockito.MockitoAnnotations
import org.testeditor.aml.AmlFactory
import org.testeditor.aml.InteractionType
import org.testeditor.aml.MethodReference
import org.testeditor.aml.dsl.AmlStandaloneSetup
import org.testeditor.aml.impl.AmlFactoryImpl
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest
import org.testeditor.tcl.util.TclModelUtil

import static org.mockito.Matchers.*

import static extension org.mockito.Mockito.*

class TclMissingFixtureValidatorTest extends AbstractParserTest {

	@Mock TclModelUtil tclModelUtil // injected into class under test
	@InjectMocks TclValidator tclValidator // class under test
	@Mock JvmParameterizedTypeReference typeReferenceMock
	@Mock ValidationMessageAcceptor messageAcceptor

	var AmlFactory amlFactory

	val message = ArgumentCaptor.forClass(String)

	@Before
	override void setUp() {
		super.setUp
		MockitoAnnotations.initMocks(this)

		val injector = (new AmlStandaloneSetup).createInjectorAndDoEMFRegistration
		amlFactory = injector.getInstance(AmlFactoryImpl)

		val operationMock = mock(JvmOperation)
		val jvmTypeMock = mock(JvmType)
		val interactionTypeMock = mock(InteractionType)
		val methodReferenceMock = mock(MethodReference)

		when(tclModelUtil.getInteraction(anyObject)).thenReturn(interactionTypeMock)
		when(interactionTypeMock.defaultMethod).thenReturn(methodReferenceMock)
		when(methodReferenceMock.operation).thenReturn(operationMock)
		when(methodReferenceMock.typeReference).thenReturn(typeReferenceMock)
		when(typeReferenceMock.type).thenReturn(jvmTypeMock)

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
		when(typeReferenceMock.type).thenReturn(null)

		// when
		tclValidator.checkFixtureMethodForExistence(tclFix.steps.head.contexts.head.steps.head)

		// then
		messageAcceptor.verify.acceptInfo(message.capture, anyObject, anyObject, anyInt, anyString)
		assertMatches(message.value, ".*could not resolve fixture")
	}

}
