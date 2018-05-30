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
		try {
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
			    try {
			      String IDvar0=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "test something", IDvar0, TestRunReporter.Status.STARTED, variables());
			      String IDvar1=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.COMPONENT, "dummyComponent", IDvar1, TestRunReporter.Status.STARTED, variables());
			      String IDvar2=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "myJsonObject = Read jsonObject from <dummyElement> [com.google.gson.JsonObject]", IDvar2, TestRunReporter.Status.STARTED, variables());
			      com.google.gson.JsonObject myJsonObject = dummyFixture.getJsonObject("dummyLocator");
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "myJsonObject = Read jsonObject from <dummyElement> [com.google.gson.JsonObject]", IDvar2, TestRunReporter.Status.OK, variables());
			      String IDvar3=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "myVal = Read value from <dummyElement> [java.lang.String]", IDvar3, TestRunReporter.Status.STARTED, variables());
			      java.lang.String myVal = dummyFixture.getValue("dummyLocator");
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "myVal = Read value from <dummyElement> [java.lang.String]", IDvar3, TestRunReporter.Status.OK, variables());
			      String IDvar4=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Start application @myJsonObject.\"my key\"", IDvar4, TestRunReporter.Status.STARTED, variables("myJsonObject.\"my key\"", myJsonObject.getAsJsonObject().get("my key").getAsJsonPrimitive().getAsString()));
			      org.junit.Assert.assertTrue("Parameter is expected to be of type = 'java.lang.String' but a non coercible value = '"+myJsonObject.getAsJsonObject().get("my key").toString()+"' was passed through variable reference = 'myJsonObject'.", myJsonObject.getAsJsonObject().get("my key").getAsJsonPrimitive().isString());
			      dummyFixture.startApplication(myJsonObject.getAsJsonObject().get("my key").getAsJsonPrimitive().getAsString());
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "Start application @myJsonObject.\"my key\"", IDvar4, TestRunReporter.Status.OK, variables("myJsonObject.\"my key\"", myJsonObject.getAsJsonObject().get("my key").getAsJsonPrimitive().getAsString()));
			      String IDvar5=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Start application @myVal", IDvar5, TestRunReporter.Status.STARTED, variables("myVal", myVal));
			      dummyFixture.startApplication(myVal);
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "Start application @myVal", IDvar5, TestRunReporter.Status.OK, variables("myVal", myVal));
			      String IDvar6=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Type confidential @confEnvVar into <Input>", IDvar6, TestRunReporter.Status.STARTED, variables());
			      dummyFixture.typeConfidentialInformationInto("some", org.testeditor.dsl.common.testing.DummyLocatorStrategy.ID, env_confEnvVar);
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "Type confidential @confEnvVar into <Input>", IDvar6, TestRunReporter.Status.OK, variables());
			      String IDvar7=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Type @nonConfEnvVar into <Input>", IDvar7, TestRunReporter.Status.STARTED, variables("nonConfEnvVar", env_nonConfEnvVar));
			      dummyFixture.typeInto("some", org.testeditor.dsl.common.testing.DummyLocatorStrategy.ID, env_nonConfEnvVar);
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "Type @nonConfEnvVar into <Input>", IDvar7, TestRunReporter.Status.OK, variables("nonConfEnvVar", env_nonConfEnvVar));
			      reporter.leave(TestRunReporter.SemanticUnit.COMPONENT, "dummyComponent", IDvar1, TestRunReporter.Status.OK, variables());
			      reporter.leave(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "test something", IDvar0, TestRunReporter.Status.OK, variables());
			      finishedTestWith(TestRunReporter.Status.OK); // reaching this line of code means successful test execution
			    } catch (AssertionError e) {
			      reporter.reportAssertionExit(e);
			      finishedTestWith(TestRunReporter.Status.ERROR);
			      org.junit.Assert.fail(e.getMessage());
			    } catch (org.testeditor.fixture.core.FixtureException e) {
			      reporter.reportFixtureExit(e);
			      finishedTestWith(TestRunReporter.Status.ABORTED);
			      org.junit.Assert.fail(e.getMessage());
			    } catch (Exception e) {
			      reporter.reportExceptionExit(e);
			      finishedTestWith(TestRunReporter.Status.ABORTED);
			      org.junit.Assert.fail(e.getMessage());
			    }
			  }
			}
		'''.toString)		
		} catch (Exception e) {
		}
	}

}
