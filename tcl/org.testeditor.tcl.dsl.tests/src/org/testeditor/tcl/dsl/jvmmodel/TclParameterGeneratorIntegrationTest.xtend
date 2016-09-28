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
			
			import java.util.Map;
			import org.testeditor.dsl.common.testing.DummyFixture;
			import org.testeditor.fixture.core.AbstractTestCase;
			import org.testeditor.fixture.core.TestStepMethod;
			
			/**
			 * Generated from MyTest.tcl
			 */
			@SuppressWarnings("all")
			public class MyTest extends AbstractTestCase {
			  private DummyFixture dummyFixture = new DummyFixture();
			  
			  private Map<?, ?> myMap;
			  
			  private String myVal;
			  
			  @TestStepMethod({ "1", "Read map from <dummyElement>" })
			  public void Read_map_from__dummyElement_1() throws Exception {
			    
			    logger.trace(" [test step] -myMap =  Read map from <dummyElement>");
			    myMap = dummyFixture.getMap("dummyLocator");
			  }
			  
			  @TestStepMethod({ "2", "Read value from <dummyElement>" })
			  public void Read_value_from__dummyElement_2() throws Exception {
			    
			    logger.trace(" [test step] -myVal =  Read value from <dummyElement>");
			    myVal = dummyFixture.getValue("dummyLocator");
			  }
			  
			  @TestStepMethod({ "3", "Start application @myMap.\"my key\"" })
			  public void Start_application__myMap__my_key_3() throws Exception {
			    
			    logger.trace(" [test step] - Start application @myMap.\"my key\"");
			    dummyFixture.startApplication(myMap.get("my key").toString());
			  }
			  
			  @TestStepMethod({ "4", "Start application @myVal" })
			  public void Start_application__myVal4() throws Exception {
			    
			    logger.trace(" [test step] - Start application @myVal");
			    dummyFixture.startApplication(myVal);
			  }
			}
		'''.toString)
	}

}
