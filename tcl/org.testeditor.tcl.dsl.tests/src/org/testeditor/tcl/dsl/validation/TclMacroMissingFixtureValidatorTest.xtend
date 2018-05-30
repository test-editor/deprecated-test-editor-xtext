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
package org.testeditor.tcl.dsl.validation

import org.eclipse.xtext.common.types.JvmParameterizedTypeReference
import org.eclipse.xtext.common.types.JvmType
import org.junit.Before
import org.junit.Test
import org.mockito.Mock
import org.testeditor.aml.InteractionType
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.TestStep

import static org.mockito.Matchers.*

import static extension org.mockito.Mockito.*
import org.eclipse.emf.common.util.BasicEList
import org.eclipse.xtext.common.types.JvmTypeReference

class TclMacroMissingFixtureValidatorTest extends AbstractMockedTclValidatorTest {

	@Mock JvmParameterizedTypeReference typeReferenceMock
	
	@Before
	def void initMocks() {
		val jvmTypeMock = JvmType.mock
		val interactionTypeMock = InteractionType.mock(RETURNS_DEEP_STUBS)

		when(tclModelUtil.getInteraction(anyObject)).thenReturn(interactionTypeMock)
		when(tclModelUtil.hasComponentContext(anyObject)).thenReturn(true)
		when(interactionTypeMock.defaultMethod.typeReference).thenReturn(typeReferenceMock)
		when(typeReferenceMock.type).thenReturn(jvmTypeMock) // default is != null => fixture exists
	}

	@Test
	def void noInfoOnExistingFixture() {
		// given
		val tmlFix = parseTcl('''
			package pa
			# MacroCollection
			
			## UnnamedMacro
			template = "hello"
			Component: some_fantasy_component
			- test step that maps
		''')
		val macro = tmlFix.macroCollection.macros.head
		val testStepThatMaps = macro.contexts.head.assertInstanceOf(
			ComponentTestStepContext).steps.head.assertInstanceOf(TestStep)
			
		// make sure that exception is "there" as expected
		val jvmTypeReferenceMock = JvmTypeReference.mock
		when(tclModelUtil.getInteraction(testStepThatMaps).defaultMethod.operation.exceptions).thenReturn(new BasicEList(#[jvmTypeReferenceMock]))
		when(jvmTypeReferenceMock.qualifiedName).thenReturn('org.testeditor.fixture.core.FixtureException')

		// when
		tclValidator.checkFixtureMethodForExistence(testStepThatMaps)

		// then
		messageAcceptor.verify(never).acceptInfo(anyString, anyObject, anyObject, anyInt, anyString)
	}

	@Test
	def void infoOnMissingFixture() {
		// given
		val tmlFix = parseTcl('''
			package pa
			# MacroCollection
			
			## UnnamedMacro
			template = "hello"
			Component: some_fantasy_component
			- test step that does not map
		''')
		val testStepThatDoesNotMap = tmlFix.macroCollection.macros.head.contexts.head.assertInstanceOf(
			ComponentTestStepContext).steps.head.assertInstanceOf(TestStep)
		when(typeReferenceMock.type).thenReturn(null)

		// when
		tclValidator.checkFixtureMethodForExistence(testStepThatDoesNotMap)

		// then
		messageAcceptor.verify.acceptInfo(message.capture, anyObject, anyObject, anyInt, anyString)
		assertMatches(message.value, ".*could not resolve fixture")
	}

}
