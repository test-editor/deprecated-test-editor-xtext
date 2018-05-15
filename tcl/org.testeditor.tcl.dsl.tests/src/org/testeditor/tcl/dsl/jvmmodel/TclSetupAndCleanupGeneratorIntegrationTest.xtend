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
			    
			    String IDvar0=getNewId(); reporter.enter(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "setup", IDvar0, "?", variables());
			    String IDvar1=getNewId(); reporter.enter(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication", IDvar1, "?", variables());
			    String IDvar2=getNewId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Start application \"org.testeditor.swing.exammple.Greetings\"", IDvar2, "?", variables());
			    dummyFixture.startApplication("org.testeditor.swing.exammple.Greetings");
			    reporter.leave(TestRunReporter.SemanticUnit.STEP, "Start application \"org.testeditor.swing.exammple.Greetings\"", IDvar2, "OK", variables());
			    reporter.leave(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication", IDvar1, "OK", variables());
			    reporter.leave(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "setup", IDvar0, "OK", variables());
			  }
			  
			  @After
			  public void cleanup() throws Exception {
			    
			    String IDvar3=getNewId(); reporter.enter(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "cleanup", IDvar3, "?", variables());
			    String IDvar4=getNewId(); reporter.enter(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication", IDvar4, "?", variables());
			    String IDvar5=getNewId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Stop application", IDvar5, "?", variables());
			    dummyFixture.stopApplication();
			    reporter.leave(TestRunReporter.SemanticUnit.STEP, "Stop application", IDvar5, "OK", variables());
			    reporter.leave(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication", IDvar4, "OK", variables());
			    reporter.leave(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "cleanup", IDvar3, "OK", variables());
			  }
			  
			  @Test
			  public void execute() throws Exception {
			    
			    String IDvar6=getNewId(); reporter.enter(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "Test Step", IDvar6, "?", variables());
			    reporter.leave(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "Test Step", IDvar6, "OK", variables());
			  }
			}
		'''.toString)
	}

}
