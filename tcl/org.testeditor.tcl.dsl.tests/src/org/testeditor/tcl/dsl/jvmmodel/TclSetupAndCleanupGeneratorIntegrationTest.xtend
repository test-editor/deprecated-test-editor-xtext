package org.testeditor.tcl.dsl.jvmmodel

import org.junit.Before
import org.junit.Test
import org.testeditor.dsl.common.testing.DummyFixture

class TclSetupAndCleanupGeneratorIntegrationTest extends AbstractTclGeneratorIntegrationTest {

	@Before
	def void parseAmlModel() {
		parseAml(DummyFixture.amlModel)
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
		val tclModel = parseTcl(tcl, 'SimpleTest.tcl')
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
			import org.testeditor.fixture.core.AbstractTestCase;
			
			/**
			 * Generated from SimpleTest.tcl
			 */
			@SuppressWarnings("all")
			public class SimpleTest extends AbstractTestCase {
			  private DummyFixture dummyFixture = new DummyFixture();
			  
			  @Before
			  public void setup() throws Exception {
			    
			    logger.trace(" [Component] GreetingApplication");
			    
			    logger.trace(" [test step] - Start application \"org.testeditor.swing.exammple.Greetings\"");
			    dummyFixture.startApplication("org.testeditor.swing.exammple.Greetings");
			  }
			  
			  @After
			  public void cleanup() throws Exception {
			    
			    logger.trace(" [Component] GreetingApplication");
			    
			    logger.trace(" [test step] - Stop application");
			    dummyFixture.stopApplication();
			  }
			  
			  @Test
			  public void execute() throws Exception {
			    
			    logger.info(" [Test specification] * Test Step);
			    
			  }
			}
		'''.toString)
	}

}
