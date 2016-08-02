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

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.junit.Before
import org.testeditor.aml.Component
import org.testeditor.aml.dsl.AmlStandaloneSetup
import org.testeditor.aml.dsl.tests.common.AmlTestModels
import org.testeditor.dsl.common.testing.ResourceSetHelper
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest

abstract class AbstractUnmockedTclValidatorTest extends AbstractParserTest {
	
	@Inject protected TclValidator tclValidator // class under test (not mocked)
	@Inject protected ValidationTestHelper validator
	@Inject protected AmlTestModels amlTestModels
	
	@Inject extension ResourceSetHelper

	protected var Component dummyComponent

	@Before
	def void setup() {
		new AmlStandaloneSetup().createInjectorAndDoEMFRegistration // needs to be registered to register aml models

		// build component "Dummy" with two interactions, "start" with a string parameter, "wait" with a long parameter
		val amlModel = amlTestModels.dummyComponent(resourceSet)
		amlModel.register("Test.aml")

		dummyComponent = amlModel.components.findFirst[name == amlTestModels.COMPONENT_NAME]
	}

	/** 
	 * register the given model with the resource set (for cross linking)
	 */
	protected def <T extends EObject> T register(T model, String fileName) {
		model.register(resourceSet, fileName)
	}
}
