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

import org.eclipse.xtext.common.types.JvmParameterizedTypeReference
import org.eclipse.xtext.common.types.JvmType
import org.junit.Test
import org.mockito.Mock
import org.testeditor.aml.InteractionType
import org.testeditor.tml.ComponentTestStepContext

import static org.mockito.Matchers.*

import static extension org.mockito.Mockito.*
import org.junit.Before

class TmlMissingFixtureValidatorTest extends AbstractTmlValidatorTest {

	@Mock JvmParameterizedTypeReference typeReferenceMock

	@Before
	def void initMocks() {
		val jvmTypeMock = JvmType.mock
		val interactionTypeMock = InteractionType.mock(RETURNS_DEEP_STUBS)

		when(tmlModelUtil.getInteraction(anyObject)).thenReturn(interactionTypeMock)
		when(tmlModelUtil.hasComponentContext(anyObject)).thenReturn(true)
		when(interactionTypeMock.defaultMethod.typeReference).thenReturn(typeReferenceMock)
		when(typeReferenceMock.type).thenReturn(jvmTypeMock) // default is != null => fixture exists
	}

	@Test
	def void noInfoOnExistingFixture() {
		// given
		val tmlFix = parse('''
			package pa
			# MacroCollection
			
			## UnnamedMacro
			template = "hello"
			Component: some_fantasy_component
			- test step that maps
		''')
		val testStepThatMaps = tmlFix.macroCollection.macros.head.contexts.head.assertInstanceOf(
			ComponentTestStepContext).steps.head

		// when
		tmlValidator.checkFixtureMethodForExistence(testStepThatMaps)

		// then
		messageAcceptor.verify(never).acceptInfo(anyString, anyObject, anyObject, anyInt, anyString)
	}

	@Test
	def void infoOnMissingFixture() {
		// given
		val tmlFix = parse('''
			package pa
			# MacroCollection
			
			## UnnamedMacro
			template = "hello"
			Component: some_fantasy_component
			- test step that does not map
		''')
		val testStepThatDoesNotMap = tmlFix.macroCollection.macros.head.contexts.head.assertInstanceOf(
			ComponentTestStepContext).steps.head
		when(typeReferenceMock.type).thenReturn(null)

		// when
		tmlValidator.checkFixtureMethodForExistence(testStepThatDoesNotMap)

		// then
		messageAcceptor.verify.acceptInfo(message.capture, anyObject, anyObject, anyInt, anyString)
		assertMatches(message.value, ".*could not resolve fixture")
	}

	@Test
	def void noInfoOnAssertion() {
		// given
		val tmlFix = parse('''
			package pa
			# MacroCollection
			
			## UnnamedMacro
			template = "hello"
			Component: some_fantasy_component
			- variable = get some
			- assert variable = "Hello"
		''')
		val testStepThatDoesNotMap = tmlFix.macroCollection.macros.head.contexts.head.assertInstanceOf(
			ComponentTestStepContext).steps.get(1)
		when(typeReferenceMock.type).thenReturn(null)

		// when
		tmlValidator.checkFixtureMethodForExistence(testStepThatDoesNotMap)

		// then
		messageAcceptor.verify(never).acceptInfo(anyString, anyObject, anyObject, anyInt, anyString)
	}
}
