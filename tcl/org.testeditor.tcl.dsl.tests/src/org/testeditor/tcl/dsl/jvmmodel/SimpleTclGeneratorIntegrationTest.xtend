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

import org.junit.Test
import org.testeditor.dsl.common.testing.DummyFixture

import static extension org.eclipse.emf.common.util.URI.createFileURI

class SimpleTclGeneratorIntegrationTest extends AbstractTclGeneratorIntegrationTest {

	// TODO extract this to DummyFixture.getAmlModel()
	val aml = '''
		package com.example
		
		import «DummyFixture.name»
		
		component type Application {
			interactions = start, stop
		}
		 
		interaction type start {
			template = "Starte Anwendung" ${path}
			method = «DummyFixture.simpleName».startApplication(path)
		}
		interaction type stop {
			template = "Stoppe Anwendung"
			method = «DummyFixture.simpleName».stopApplication()
		}
		 
		interaction type getValue {
			template = "Lese Wert von" ${element}
			method = «DummyFixture.simpleName».getValue(element)
		}
		 
		interaction type setValue {
			template = "Setze Wert von" ${element} "auf" ${value} "."
			method = «DummyFixture.simpleName».setValue(element, value)
		}
		
		interaction type getList {
			template = "Lese Liste von" ${element}
			method = «DummyFixture.simpleName».getList(element)
		}
		
		element type Label {
			interactions = getList
		}
					
		component GreetingApplication is Application {
			element bar is Label {
				label = "Label"
				locator = "label.greet"
			}
		}
	'''

	@Test
	def void testDefaultGeneration() {
		// given
		val tcl = '''
			package com.example
			
			# SimpleTest
			* Start the famous greetings application
				Mask: GreetingApplication
				- Starte Anwendung "org.testeditor.swing.exammple.Greetings"
				- foo = Lese Liste von <bar>
				- Stoppe Anwendung
			
			* Do something different
		'''
		val amlModel = amlParseHelper.parse(aml, resourceSet)
		val tclModel = tclParseHelper.parse(tcl, 'SimpleTest.tcl'.createFileURI, resourceSet)
		amlModel.assertNoSyntaxErrors
		tclModel.assertNoSyntaxErrors

		// when
		val generatedCode = generate(tclModel)

		// then
		generatedCode.assertEquals('''
			package com.example;
			
			import org.junit.Test;
			import org.testeditor.dsl.common.testing.DummyFixture;
			
			/**
			 * Generated from SimpleTest.tcl
			 */
			@SuppressWarnings("all")
			public class SimpleTest {
			  private DummyFixture dummyFixture = new DummyFixture();
			  
			  @Test
			  public void execute() throws Exception {
			    
			    /* Start the famous greetings application */
			    
			    // Component: GreetingApplication
			    
			    // - Starte Anwendung "org.testeditor.swing.exammple.Greetings"
			    dummyFixture.startApplication("org.testeditor.swing.exammple.Greetings");
			    // - Lese Liste von <bar>
			    java.util.List<? extends java.lang.Object> foo = dummyFixture.getList("label.greet");
			    // - Stoppe Anwendung
			    dummyFixture.stopApplication();
			    /* Do something different */
			    
			  }
			}
		'''.toString.replaceAll('\r\n', '\n'))
	}

	@Test
	def void testGenerationWithBeforeAndAfter() {
		// given
		val tcl = '''
			package com.example
			
			# SimpleTest
			
			Setup:
				Component: GreetingApplication
				- Starte Anwendung "org.testeditor.swing.exammple.Greetings"
			
			Cleanup:
				Component: GreetingApplication
				- Stoppe Anwendung
			
			* Test Step
				Mask: GreetingApplication
				- foo = Lese Liste von <bar>
		'''
		val amlModel = amlParseHelper.parse(aml, resourceSet)
		val tclModel = tclParseHelper.parse(tcl, 'SimpleTest.tcl'.createFileURI, resourceSet)
		amlModel.assertNoSyntaxErrors
		tclModel.assertNoSyntaxErrors
		
		// when
		val generatedCode = tclModel.generate
		
		// then
		generatedCode.assertEquals('''
			package com.example;
			
			import org.junit.After;
			import org.junit.Before;
			import org.junit.Test;
			import org.testeditor.dsl.common.testing.DummyFixture;
			
			/**
			 * Generated from SimpleTest.tcl
			 */
			@SuppressWarnings("all")
			public class SimpleTest {
			  private DummyFixture dummyFixture = new DummyFixture();
			  
			  @Before
			  public void setup() throws Exception {
			    
			    // Component: GreetingApplication
			    
			    // - Starte Anwendung "org.testeditor.swing.exammple.Greetings"
			    dummyFixture.startApplication("org.testeditor.swing.exammple.Greetings");
			  }
			  
			  @After
			  public void cleanup() throws Exception {
			    
			    // Component: GreetingApplication
			    
			    // - Stoppe Anwendung
			    dummyFixture.stopApplication();
			  }
			  
			  @Test
			  public void execute() throws Exception {
			    
			    /* Test Step */
			    
			    // Component: GreetingApplication
			    
			    // - Lese Liste von <bar>
			    java.util.List<? extends java.lang.Object> foo = dummyFixture.getList("label.greet");
			  }
			}
		'''.toString.replaceAll('\r\n', '\n'))
	}

}
