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
			import org.testeditor.fixture.core.TestRunReporter;
			
			/**
			 * Generated from SimpleTest.tcl
			 */
			@SuppressWarnings("all")
			public class SimpleTest extends AbstractTestCase {
			  private DummyFixture dummyFixture = new DummyFixture();
			  
			  @Before
			  public void setup() throws Exception {
			    
			    reporter.enter(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication");
			    
			    reporter.enter(TestRunReporter.SemanticUnit.STEP, "Start application \"org.testeditor.swing.exammple.Greetings\"");
			    dummyFixture.startApplication("org.testeditor.swing.exammple.Greetings");
			  }
			  
			  @After
			  public void cleanup() throws Exception {
			    
			    reporter.enter(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication");
			    
			    reporter.enter(TestRunReporter.SemanticUnit.STEP, "Stop application");
			    dummyFixture.stopApplication();
			  }
			  
			  @Test
			  public void execute() throws Exception {
			    
			    reporter.enter(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "Test Step");
			    
			  }
			}
		'''.toString)
	}

}
