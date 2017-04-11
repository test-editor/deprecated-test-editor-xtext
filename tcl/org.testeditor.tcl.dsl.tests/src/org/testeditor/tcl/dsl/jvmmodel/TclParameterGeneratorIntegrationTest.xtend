package org.testeditor.tcl.dsl.jvmmodel

import org.junit.Before
import org.junit.Test
import org.testeditor.dsl.common.testing.DummyFixture

class TclParameterGeneratorIntegrationTest extends AbstractTclGeneratorIntegrationTest {

	@Before
	def void parseAmlModel() {
		parseAml(DummyFixture.amlModel + '''
			component type dummyComponentType {
				interactions = start, getMap, getValue
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
			- myMap = Read map from <dummyElement>
			- myVal = Read value from <dummyElement>
			- Start application @myMap."my key"
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
			    
			    reporter.enter(TestRunReporter.SemanticUnit.STEP, "java.util.Map<java.lang.String, java.lang.String> myMap = Read map from <dummyElement>");
			    java.util.Map<java.lang.String, java.lang.String> myMap = dummyFixture.getMap("dummyLocator");
			    reporter.enter(TestRunReporter.SemanticUnit.STEP, "java.lang.String myVal = Read value from <dummyElement>");
			    java.lang.String myVal = dummyFixture.getValue("dummyLocator");
			    reporter.enter(TestRunReporter.SemanticUnit.STEP, "Start application @myMap.\"my key\"");
			    dummyFixture.startApplication(String.valueOf(myMap.get("my key")));
			    reporter.enter(TestRunReporter.SemanticUnit.STEP, "Start application @myVal");
			    dummyFixture.startApplication(myVal);
			  }
			}
		'''.toString)
	}

}
