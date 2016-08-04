package org.testeditor.tcl.dsl.jvmmodel

import org.eclipse.xtext.util.JavaVersion
import org.eclipse.xtext.xbase.compiler.OnTheFlyJavaCompiler2
import org.junit.Before
import org.junit.Test
import org.testeditor.dsl.common.testing.DummyFixture

import static extension org.eclipse.emf.common.util.URI.createFileURI
import javax.inject.Inject
import org.testeditor.dsl.common.testing.ResourceSetHelper

class TclSetupAndCleanupGeneratorIntegrationTest extends AbstractTclGeneratorIntegrationTest {

	@Inject extension ResourceSetHelper	

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

		val configModel = tclParseHelper.parse(config, 'MyConfig.config'.createFileURI, resourceSet)
		configModel.assertNoSyntaxErrors
		
		// TODO improve this code...
		val configCode = configModel.generate
		val javaCompiler = new OnTheFlyJavaCompiler2(class.classLoader, JavaVersion.JAVA8)
		val myConfigClass = javaCompiler.compileToClass('com.example.MyConfig', configCode)
		val newResourceSet = resourceSetProvider.get
		newResourceSet.resources += configModel.eResource
		amlParseHelper.parse(DummyFixture.amlModel, newResourceSet)
		newResourceSet.classpathURIContext = myConfigClass.classLoader
		
		// when
		val tclModel = tclParseHelper.parse(tcl, 'SimpleTest.tcl'.createFileURI, newResourceSet)
		tclModel.assertNoSyntaxErrors
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
		'''.toString.replaceAll('\r\n', '\n'))
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
		'''.toString.replaceAll('\r\n', '\n'))
	}

}
