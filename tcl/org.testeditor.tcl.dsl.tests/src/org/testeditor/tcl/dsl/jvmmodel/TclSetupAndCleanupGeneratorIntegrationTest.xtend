package org.testeditor.tcl.dsl.jvmmodel

import org.junit.Before
import org.junit.Test
import org.testeditor.dsl.common.testing.DummyFixture

import static extension org.eclipse.emf.common.util.URI.createFileURI

class TclSetupAndCleanupGeneratorIntegrationTest extends AbstractTclGeneratorIntegrationTest {

	@Before
	def void parseAmlModel() {
		val amlModel = amlParseHelper.parse(DummyFixture.amlModel, resourceSet)
		amlModel.assertNoSyntaxErrors
	}

	@Test
	def void testGenerationWithBeforeAndAfter() {
		// given
		val tcl = '''
			package com.example
			
			# SimpleTest
			
			Setup:
				Component: GreetingApplication
				- Start application "org.testeditor.swing.exammple.Greetings"
			
			Cleanup:
				Component: GreetingApplication
				- Stop application
			
			* Test Step
		'''
		val tclModel = tclParseHelper.parse(tcl, 'SimpleTest.tcl'.createFileURI, resourceSet)
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
			    
			    // - Start application "org.testeditor.swing.exammple.Greetings"
			    dummyFixture.startApplication("org.testeditor.swing.exammple.Greetings");
			  }
			  
			  @After
			  public void cleanup() throws Exception {
			    
			    // Component: GreetingApplication
			    
			    // - Stop application
			    dummyFixture.stopApplication();
			  }
			  
			  @Test
			  public void execute() throws Exception {
			    
			    /* Test Step */
			    
			  }
			}
		'''.toString.replaceAll('\r\n', '\n'))
	}

}