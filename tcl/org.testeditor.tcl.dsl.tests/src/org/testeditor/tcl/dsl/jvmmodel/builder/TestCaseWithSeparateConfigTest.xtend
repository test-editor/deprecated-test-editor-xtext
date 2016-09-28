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
package org.testeditor.tcl.dsl.jvmmodel.builder

import org.junit.Test
import org.testeditor.dsl.common.testing.DummyFixture
import org.testeditor.fixture.core.AbstractTestCase
import org.junit.Ignore

class TestCaseWithSeparateConfigTest extends AbstractStandaloneBuilderTest {

	@Ignore
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
			
			@SuppressWarnings("all")
			public abstract class MyConfig extends AbstractTestCase {
			  protected DummyFixture dummyFixture = new DummyFixture();
			  
			  @Before
			  public void setupMyConfig() throws Exception {
			    
			    logger.trace(" [Component] GreetingApplication");
			    
			    logger.trace(" [test step] - Start application \"org.testeditor.swing.exammple.Greetings\"");
			    dummyFixture.startApplication("org.testeditor.swing.exammple.Greetings");
			  }
			  
			  @After
			  public void cleanupMyConfig() throws Exception {
			    
			    logger.trace(" [Component] GreetingApplication");
			    
			    logger.trace(" [test step] - Stop application");
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
			    
			    logger.info(" [Test specification] * Test Step");
			    
			    logger.trace(" [Component] GreetingApplication");
			    
			    logger.trace(" [test step] - Wait for \"3\" seconds");
			    dummyFixture.waitSeconds(3);
			  }
			}
		'''.toString)
	}

}
