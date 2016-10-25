package org.testeditor.tcl.dsl.jvmmodel.builder

import org.junit.Test
import org.testeditor.dsl.common.testing.DummyFixture
import org.testeditor.fixture.core.AbstractTestCase

class TestCaseWithSeparateConfigTest extends AbstractStandaloneBuilderTest {

	@Test
	def void generateWithSeparateConfiguration() {
		// given
		val configSource = '''
			package com.example
			
			config MyConfig
			
			Setup:
				Component: GreetingApplication
				- Start application "org.testeditor.swing.exammple.Greetings"
			
			Cleanup:
				Component: GreetingApplication
				- Stop application
		'''
		val testSource = '''
			package com.example
						
			# SimpleTest
			
			config MyConfig
			
			* Test Step
				Component: GreetingApplication
				- Wait for "3" seconds
		'''
		writeFile("src/com/example/Dummy.aml", DummyFixture.amlModel)
		writeFile("src/com/example/MyConfig.config", configSource)
		writeFile("src/com/example/SimpleTest.tcl", testSource)
		classPathEntries += DummyFixture.classPathEntry
		classPathEntries += AbstractTestCase.classPathEntry

		// when
		builder.launch

		// then
		val configOutput = readFile("src-gen/com/example/MyConfig.java").removeJavaDoc
		val testOutput = readFile("src-gen/com/example/SimpleTest.java").removeJavaDoc
		configOutput.assertEquals('''
			package com.example;
			
			import org.junit.After;
			import org.junit.Before;
			import org.testeditor.dsl.common.testing.DummyFixture;
			import org.testeditor.fixture.core.AbstractTestCase;
			import org.testeditor.fixture.core.TestRunReporter;
			
			@SuppressWarnings("all")
			public abstract class MyConfig extends AbstractTestCase {
			  protected DummyFixture dummyFixture = new DummyFixture();
			  
			  @Before
			  public void setupMyConfig() throws Exception {
			    
			    reporter.enter(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication");
			    
			    reporter.enter(TestRunReporter.SemanticUnit.STEP, "Start application \"org.testeditor.swing.exammple.Greetings\"");
			    dummyFixture.startApplication("org.testeditor.swing.exammple.Greetings");
			  }
			  
			  @After
			  public void cleanupMyConfig() throws Exception {
			    
			    reporter.enter(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication");
			    
			    reporter.enter(TestRunReporter.SemanticUnit.STEP, "Stop application");
			    dummyFixture.stopApplication();
			  }
			}
		'''.toString)
		testOutput.assertEquals('''
			package com.example;
			
			import com.example.MyConfig;
			import org.junit.Test;
			import org.testeditor.fixture.core.TestRunReporter;
			
			@SuppressWarnings("all")
			public class SimpleTest extends MyConfig {
			  @Test
			  public void execute() throws Exception {
			    
			    reporter.enter(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "Test Step");
			    
			    reporter.enter(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication");
			    
			    reporter.enter(TestRunReporter.SemanticUnit.STEP, "Wait for \"3\" seconds");
			    dummyFixture.waitSeconds(3);
			  }
			}
		'''.toString)
	}

}
