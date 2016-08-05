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
			
			/**
			 * Generated from MyTest.tcl
			 */
			@SuppressWarnings("all")
			public class MyTest {
			  private DummyFixture dummyFixture = new DummyFixture();
			  
			  @Test
			  public void execute() throws Exception {
			    
			    /* test something */
			    
			    // Component: dummyComponent
			    
			    // - Read map from <dummyElement>
			    java.util.Map<? extends java.lang.Object, ? extends java.lang.Object> myMap = dummyFixture.getMap("dummyLocator");
			    // - Read value from <dummyElement>
			    java.lang.String myVal = dummyFixture.getValue("dummyLocator");
			    // - Start application @myMap."my key"
			    dummyFixture.startApplication(myMap.get("my key").toString());
			    // - Start application @myVal
			    dummyFixture.startApplication(myVal);
			  }
			}
		'''.toString)
	}

}