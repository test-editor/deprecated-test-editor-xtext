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
import org.junit.Before
import org.junit.Test
import org.testeditor.aml.dsl.AmlStandaloneSetup
import org.testeditor.aml.dsl.tests.AmlModelGenerator
import org.testeditor.aml.dsl.tests.common.AmlTestModels
import org.testeditor.dsl.common.testing.DummyFixture
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.dsl.jvmmodel.SimpleTypeComputer
import org.testeditor.tcl.dsl.jvmmodel.TclTypeUsageComputer
import org.testeditor.tcl.dsl.services.TclGrammarAccess
import org.testeditor.tcl.dsl.tests.TclModelGenerator
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest
import org.testeditor.tsl.StepContentValue

class TclTypeValidationUtilTest  extends AbstractParserTest {

	@Inject SimpleTypeComputer simpleTypeComputer // class under test

	@Inject protected TclGrammarAccess grammarAccess

	@Before
	def void setup() {		
		(new AmlStandaloneSetup).createInjectorAndDoEMFRegistration
	}

}