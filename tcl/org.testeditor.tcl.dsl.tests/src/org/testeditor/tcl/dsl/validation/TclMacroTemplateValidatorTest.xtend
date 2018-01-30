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

import javax.inject.Inject
import org.junit.Test
import org.mockito.Mock
import org.testeditor.aml.dsl.tests.AmlModelGenerator
import org.testeditor.aml.dsl.validation.AmlValidator

import static org.mockito.Mockito.*

class TclMacroTemplateValidatorTest extends AbstractMockedTclValidatorTest {

	@Mock AmlValidator amlValidator
	
	@Inject extension AmlModelGenerator
	
	@Test
	def void testThatTemplateValidationIsDelegated() {
		// given
		val template = template("hello", "some")

		// when
		tclValidator.checkTemplateHoldsValidCharacters(template)

		// then
		verify(amlValidator).checkTemplateHoldsValidCharacters(template)		
	}


}
