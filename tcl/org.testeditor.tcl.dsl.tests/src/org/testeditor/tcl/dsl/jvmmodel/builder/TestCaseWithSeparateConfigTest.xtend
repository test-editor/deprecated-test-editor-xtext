package org.testeditor.tcl.dsl.jvmmodel.builder

import org.junit.Test
import org.testeditor.dsl.common.testing.DummyFixture
import org.testeditor.dsl.common.testing.DummyLocatorStrategy
import org.testeditor.fixture.core.AbstractTestCase

class TestCaseWithSeparateConfigTest extends AbstractStandaloneBuilderTest {

	@Test
	def void testGenerateWithSeparateConfiguration() {
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
		classPathEntries += DummyLocatorStrategy.classPathEntry
		classPathEntries += AbstractTestCase.classPathEntry

		// when
		builder.launch

		// then
		val configOutput = readFile("src-gen/com/example/MyConfig.java").removeJavaDoc
		val testOutput = readFile("src-gen/com/example/SimpleTest.java").removeJavaDoc
		configOutput.contains('''
			@SuppressWarnings("all")
			public abstract class MyConfig extends AbstractTestCase {
			  protected DummyFixture dummyFixture = new DummyFixture();
		'''.toString)
		configOutput.contains('''
			  @Before
			  public void setupMyConfig() throws Exception {
		'''.toString)
		configOutput.contains('''
			  @After
			  public void cleanupMyConfig() throws Exception {
		'''.toString)
		testOutput.contains('''
			@SuppressWarnings("all")
			public class SimpleTest extends MyConfig {
			  @Test
			  public void execute() throws Exception {
		'''.toString)
	}
	
	
	@Test
	def void testGenerateConfigurationWithUnknownFixture() {
		// given
		val configSource = '''
			package com.example
			
			config MyConfig
			
			Setup:
				Component: GreetingApplication
				- Start application "org.testeditor.swing.exammple.Greetings"
				- do something
			
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
		classPathEntries += DummyLocatorStrategy.classPathEntry
		classPathEntries += AbstractTestCase.classPathEntry

		// when
		builder.launch

		// then
		val configOutput = readFile("src-gen/com/example/MyConfig.java").removeJavaDoc
		configOutput.assertContains('''
			org.junit.Assert.fail("Template 'do something' cannot be resolved with any known macro/fixture. Please check your Configuration 'MyConfig' in line 8.");
		'''.toString)
	}
	

}
