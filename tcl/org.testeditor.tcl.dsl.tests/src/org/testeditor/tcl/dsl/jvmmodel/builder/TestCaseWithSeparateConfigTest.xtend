package org.testeditor.tcl.dsl.jvmmodel.builder

import org.junit.Test
import org.testeditor.dsl.common.testing.DummyFixture

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
			
			@SuppressWarnings("all")
			public abstract class MyConfig {
			  protected DummyFixture dummyFixture = new DummyFixture();
			  
			  @Before
			  public void setupMyConfig() throws Exception {
			    
			    // Component: GreetingApplication
			    
			    // - Start application "org.testeditor.swing.exammple.Greetings"
			    dummyFixture.startApplication("org.testeditor.swing.exammple.Greetings");
			  }
			  
			  @After
			  public void cleanupMyConfig() throws Exception {
			    
			    // Component: GreetingApplication
			    
			    // - Stop application
			    dummyFixture.stopApplication();
			  }
			}
		'''.toString)
		testOutput.assertEquals('''
			package com.example;
			
			import com.example.MyConfig;
			import org.junit.Test;
			
			@SuppressWarnings("all")
			public class SimpleTest extends MyConfig {
			  @Test
			  public void execute() throws Exception {
			    
			    /* Test Step */
			    
			    // Component: GreetingApplication
			    
			    // - Wait for "3" seconds
			    dummyFixture.waitSeconds(3);
			  }
			}
		'''.toString)
	}

}
