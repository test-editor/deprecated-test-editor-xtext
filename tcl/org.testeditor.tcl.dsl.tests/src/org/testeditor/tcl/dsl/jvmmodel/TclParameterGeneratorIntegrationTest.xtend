/*******************************************************************************
 * Copyright (c) 2012 - 2017 Signal Iduna Corporation and others.
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
package org.testeditor.tcl.dsl.jvmmodel

import org.junit.Before
import org.junit.Test
import org.testeditor.dsl.common.testing.DummyFixture

class TclParameterGeneratorIntegrationTest extends AbstractTclGeneratorIntegrationTest {

	@Before
	def void parseAmlModel() {
		parseAml(DummyFixture.amlModel + '''
			component type dummyComponentType {
				interactions = start, getJsonObject, getValue
			}
			component dummyComponent is dummyComponentType {
				element dummyElement is Label {
					locator = "dummyLocator"
				}
			}
		''').assertNoSyntaxErrors
	}

	@Test
	def void testGeneration() {
		val tclModel = parseTcl('''
			package com.example
			
			# MyTest
			
			* test something
			
			Component: dummyComponent
			- myJsonObject = Read jsonObject from <dummyElement>
			- myVal = Read value from <dummyElement>
			- Start application @myJsonObject."my key"
			- Start application @myVal
		''')
		tclModel.addToResourceSet
		

		val tclModelCode = tclModel.generate

		tclModelCode.assertEquals('''
			package com.example;
			
			import org.junit.Test;
			import org.testeditor.dsl.common.testing.DummyFixture;
			import org.testeditor.fixture.core.AbstractTestCase;
			import org.testeditor.fixture.core.TestRunReporter;
			
			/**
			 * Generated from MyTest.tcl
			 */
			@SuppressWarnings("all")
			public class MyTest extends AbstractTestCase {
			  private DummyFixture dummyFixture = new DummyFixture();
			  
			  @Test
			  public void execute() throws Exception {
			    
			    reporter.enter(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "test something");
			    
			    reporter.enter(TestRunReporter.SemanticUnit.COMPONENT, "dummyComponent");
			    
			    reporter.enter(TestRunReporter.SemanticUnit.STEP, "com.google.gson.JsonObject myJsonObject = Read jsonObject from <dummyElement>");
			    com.google.gson.JsonObject myJsonObject = dummyFixture.getJsonObject("dummyLocator");
			    reporter.enter(TestRunReporter.SemanticUnit.STEP, "java.lang.String myVal = Read value from <dummyElement>");
			    java.lang.String myVal = dummyFixture.getValue("dummyLocator");
			    reporter.enter(TestRunReporter.SemanticUnit.STEP, "Start application @myJsonObject.\"my key\"");
			    org.junit.Assert.assertTrue("Parameter is expected to be of type = 'java.lang.String' but a non coercible value = '"+myJsonObject.getAsJsonObject().get("my key").toString()+"' was passed through variable reference = 'myJsonObject'.", myJsonObject.getAsJsonObject().get("my key").getAsJsonPrimitive().isString());
			    dummyFixture.startApplication(myJsonObject.getAsJsonObject().get("my key").getAsJsonPrimitive().getAsString());
			    reporter.enter(TestRunReporter.SemanticUnit.STEP, "Start application @myVal");
			    dummyFixture.startApplication(myVal);
			  }
			}
		'''.toString)
	}

}
