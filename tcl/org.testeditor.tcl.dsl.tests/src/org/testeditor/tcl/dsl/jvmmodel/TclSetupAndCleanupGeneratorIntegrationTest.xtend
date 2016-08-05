package org.testeditor.tcl.dsl.jvmmodel

import org.eclipse.xtext.util.JavaVersion
import org.eclipse.xtext.xbase.compiler.OnTheFlyJavaCompiler2
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
		'''.toString)
	}

	@Test
	def void generateWithSeparateConfiguration() {
		// given
		val config = '''
			package com.example
			
			config MyConfig
			
			Setup:
				Component: GreetingApplication
				- Start application "org.testeditor.swing.exammple.Greetings"
			
			Cleanup:
				Component: GreetingApplication
				- Stop application
		'''
		val tcl = '''
			package com.example
			
			# SimpleTest
			
			config MyConfig
			
			* Test Step
				Component: GreetingApplication
				- Wait for "3" seconds
		'''

		val configModel = parseTcl(config, 'MyConfig.config')
		
		// TODO improve this code...
		val configCode = configModel.generate
		val javaCompiler = new OnTheFlyJavaCompiler2(class.classLoader, JavaVersion.JAVA8)
		val myConfigClass = javaCompiler.compileToClass('com.example.MyConfig', configCode)
		val newResourceSet = createNewResourceSet
		newResourceSet.resources += configModel.eResource
		parseAml(DummyFixture.amlModel, newResourceSet)
		newResourceSet.classpathURIContext = myConfigClass.classLoader	
		
		// when
		val tclModel = parseTcl(tcl, 'SimpleTest.tcl', newResourceSet)
		val testCode = tclModel.generate

		// then
		configCode.assertEquals('''
			package com.example;
			
			import org.junit.After;
			import org.junit.Before;
			import org.testeditor.dsl.common.testing.DummyFixture;
			
			/**
			 * Generated from MyConfig.config
			 */
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
		testCode.assertEquals('''
			package com.example;
			
			import com.example.MyConfig;
			import org.junit.Test;
			
			/**
			 * Generated from SimpleTest.tcl
			 */
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
