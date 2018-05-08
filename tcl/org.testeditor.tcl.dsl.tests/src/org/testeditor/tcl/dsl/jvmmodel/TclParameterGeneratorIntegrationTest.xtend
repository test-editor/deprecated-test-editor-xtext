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
				element Input is Text {
					locator = "some"
					locatorStrategy = DummyLocatorStrategy.ID
				}
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
			
			require        confEnvVar,
			        public nonConfEnvVar
			
			# MyTest
			
			* test something
			
			Component: dummyComponent
			- myJsonObject = Read jsonObject from <dummyElement>
			- myVal = Read value from <dummyElement>
			- Start application @myJsonObject."my key"
			- Start application @myVal
			- Type confidential @confEnvVar into <Input>
			- Type @nonConfEnvVar into <Input>
		''')
		tclModel.addToResourceSet
		

		val tclModelCode = tclModel.generate

		tclModelCode.assertEquals('''
			package com.example;
			
			import org.junit.Before;
			import org.junit.Test;
			import org.testeditor.dsl.common.testing.DummyFixture;
			import org.testeditor.fixture.core.AbstractTestCase;
			import org.testeditor.fixture.core.MaskingString;
			import org.testeditor.fixture.core.TestRunReporter;
			
			/**
			 * Generated from MyTest.tcl
			 */
			@SuppressWarnings("all")
			public class MyTest extends AbstractTestCase {
			  private DummyFixture dummyFixture = new DummyFixture();
			  
			  private MaskingString env_confEnvVar = new MaskingString(System.getenv("confEnvVar"));
			  
			  private String env_nonConfEnvVar = System.getenv("nonConfEnvVar");
			  
			  @Before
			  public void checkEnvironmentVariablesOnExistence() throws Exception {
			    org.junit.Assert.assertNotNull("environment variable 'confEnvVar' must not be null", env_confEnvVar.get());
			    org.junit.Assert.assertNotNull("environment variable 'nonConfEnvVar' must not be null", env_nonConfEnvVar);
			    
			  }
			  
			  @Test
			  public void execute() throws Exception {
			    
			    reporter.enter(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "test something");
			    
			    reporter.enter(TestRunReporter.SemanticUnit.COMPONENT, "dummyComponent");
			    
			    reporter.enter(TestRunReporter.SemanticUnit.STEP, "myJsonObject = Read jsonObject from <dummyElement> [com.google.gson.JsonObject]");
			    com.google.gson.JsonObject myJsonObject = dummyFixture.getJsonObject("dummyLocator");
			    reporter.enter(TestRunReporter.SemanticUnit.STEP, "myVal = Read value from <dummyElement> [java.lang.String]");
			    java.lang.String myVal = dummyFixture.getValue("dummyLocator");
			    reporter.enter(TestRunReporter.SemanticUnit.STEP, "Start application @myJsonObject.\"my key\" // myJsonObject = '" + myJsonObject + "'");
			    org.junit.Assert.assertTrue("Parameter is expected to be of type = 'java.lang.String' but a non coercible value = '"+myJsonObject.getAsJsonObject().get("my key").toString()+"' was passed through variable reference = 'myJsonObject'.", myJsonObject.getAsJsonObject().get("my key").getAsJsonPrimitive().isString());
			    dummyFixture.startApplication(myJsonObject.getAsJsonObject().get("my key").getAsJsonPrimitive().getAsString());
			    reporter.enter(TestRunReporter.SemanticUnit.STEP, "Start application @myVal // myVal = '" + myVal + "'");
			    dummyFixture.startApplication(myVal);
			    reporter.enter(TestRunReporter.SemanticUnit.STEP, "Type confidential @confEnvVar into <Input> // confEnvVar = '" + env_confEnvVar + "'");
			    dummyFixture.typeConfidentialInformationInto("some", org.testeditor.dsl.common.testing.DummyLocatorStrategy.ID, env_confEnvVar);
			    reporter.enter(TestRunReporter.SemanticUnit.STEP, "Type @nonConfEnvVar into <Input> // nonConfEnvVar = '" + env_nonConfEnvVar + "'");
			    dummyFixture.typeInto("some", org.testeditor.dsl.common.testing.DummyLocatorStrategy.ID, env_nonConfEnvVar);
			  }
			}
		'''.toString)
	}

}
