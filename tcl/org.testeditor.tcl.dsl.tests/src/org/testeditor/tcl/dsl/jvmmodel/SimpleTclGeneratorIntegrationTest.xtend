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
package org.testeditor.tcl.dsl.jvmmodel

import org.junit.Before
import org.junit.Test
import org.testeditor.dsl.common.testing.DummyFixture

class SimpleTclGeneratorIntegrationTest extends AbstractTclGeneratorIntegrationTest {

	@Before
	def void parseAmlModel() {
		parseAml(DummyFixture.amlModel)
	}

	@Test
	def void testMinimalGeneration() {
		// given
		val tcl = '''
			package com.example
			
			# SimpleTest
		'''
		val tclModel = parseTcl(tcl, "SimpleTest.tcl")

		// when
		val generatedCode = tclModel.generate

		// then
		generatedCode.assertEquals('''
			package com.example;
			
			import org.junit.Test;
			import org.testeditor.fixture.core.AbstractTestCase;
			
			/**
			 * Generated from SimpleTest.tcl
			 */
			@SuppressWarnings("all")
			public class SimpleTest extends AbstractTestCase {
			  @Test
			  public void execute() throws Exception {
			    
			  }
			}
		'''.toString)
	}
	
	@Test
	def void testAssertionGeneration() {
		// given
		val tcl = '''
			package com.example
			
			# SimpleTest
			* Test assertions in the famous greeting application
				Mask: GreetingApplication
				- foo = Read list from <bar>
				- baz = Read value from <bar>
				- book = Read bool from <bar>
				- mak = Read map from <bar>
				- assert foo
				- assert baz = "fix"
				- assert book
				- assert mak."key with spaces" = "fox"
				- assert ! foo
				- assert baz <> "fix"
				- assert ! book
				- assert mak."key with spaces" <> "fox"
				- assert mak."key with spaces"
				- assert ! mak."key with spaces"
				- assert baz = mak.otherkey
		'''
		val tclModel = parseTcl(tcl, "SimpleTest.tcl")

		// when
		val generatedCode = tclModel.generate

		// then
		generatedCode.assertEquals('''
			package com.example;
			
			import org.junit.Test;
			import org.testeditor.dsl.common.testing.DummyFixture;
			import org.testeditor.fixture.core.AbstractTestCase;
			
			/**
			 * Generated from SimpleTest.tcl
			 */
			@SuppressWarnings("all")
			public class SimpleTest extends AbstractTestCase {
			  private DummyFixture dummyFixture = new DummyFixture();
			  
			  @Test
			  public void execute() throws Exception {
			    
			    logger.info(" [Test specification] * Test assertions in the famous greeting application");
			    
			    logger.trace(" [Component] GreetingApplication");
			    
			    logger.trace(" [test step] -java.util.List<? extends java.lang.Object> foo =  Read list from <bar>");
			    java.util.List<? extends java.lang.Object> foo = dummyFixture.getList("label.greet");
			    logger.trace(" [test step] -java.lang.String baz =  Read value from <bar>");
			    java.lang.String baz = dummyFixture.getValue("label.greet");
			    logger.trace(" [test step] -boolean book =  Read bool from <bar>");
			    boolean book = dummyFixture.getBool("label.greet");
			    logger.trace(" [test step] -java.util.Map<? extends java.lang.Object, ? extends java.lang.Object> mak =  Read map from <bar>");
			    java.util.Map<? extends java.lang.Object, ? extends java.lang.Object> mak = dummyFixture.getMap("label.greet");
			    logger.trace("- assert foo");
			    org.junit.Assert.assertNotNull("foo", foo);
			    logger.trace("- assert baz = \"fix\"");
			    org.junit.Assert.assertEquals("baz = \"fix\"", "fix", baz);
			    logger.trace("- assert book");
			    org.junit.Assert.assertTrue("book", book);
			    logger.trace("- assert mak.\"key with spaces\" = \"fox\"");
			    org.junit.Assert.assertEquals("mak.\"key with spaces\" = \"fox\"", "fox", mak.get("key with spaces"));
			    logger.trace("- assert ! foo");
			    org.junit.Assert.assertNull("! foo", foo);
			    logger.trace("- assert baz <> \"fix\"");
			    org.junit.Assert.assertNotEquals("baz <> \"fix\"", "fix", baz);
			    logger.trace("- assert ! book");
			    org.junit.Assert.assertFalse("! book", book);
			    logger.trace("- assert mak.\"key with spaces\" <> \"fox\"");
			    org.junit.Assert.assertNotEquals("mak.\"key with spaces\" <> \"fox\"", "fox", mak.get("key with spaces"));
			    logger.trace("- assert mak.\"key with spaces\"");
			    org.junit.Assert.assertNotNull("mak.\"key with spaces\"", mak.get("key with spaces"));
			    logger.trace("- assert ! mak.\"key with spaces\"");
			    org.junit.Assert.assertNull("! mak.\"key with spaces\"", mak.get("key with spaces"));
			    logger.trace("- assert baz = mak.otherkey");
			    org.junit.Assert.assertEquals("baz = mak.otherkey", mak.get("otherkey"), baz);
			  }
			}
		'''.toString)
	}

	@Test
	def void testDefaultGeneration() {
		// given
		val tcl = '''
			package com.example
			
			# SimpleTest
			* Start the famous greetings application
				Mask: GreetingApplication
				- Start application "org.testeditor.swing.exammple.Greetings"
				- foo = Read list from <bar>
				- Stop application
			
			* Do something different
		'''
		val tclModel = parseTcl(tcl, "SimpleTest.tcl")

		// when
		val generatedCode = tclModel.generate

		// then
		generatedCode.assertEquals('''
			package com.example;
			
			import org.junit.Test;
			import org.testeditor.dsl.common.testing.DummyFixture;
			import org.testeditor.fixture.core.AbstractTestCase;
			
			/**
			 * Generated from SimpleTest.tcl
			 */
			@SuppressWarnings("all")
			public class SimpleTest extends AbstractTestCase {
			  private DummyFixture dummyFixture = new DummyFixture();
			  
			  @Test
			  public void execute() throws Exception {
			    
			    logger.info(" [Test specification] * Start the famous greetings application");
			    
			    logger.trace(" [Component] GreetingApplication");
			    
			    logger.trace(" [test step] - Start application \"org.testeditor.swing.exammple.Greetings\"");
			    dummyFixture.startApplication("org.testeditor.swing.exammple.Greetings");
			    logger.trace(" [test step] -java.util.List<? extends java.lang.Object> foo =  Read list from <bar>");
			    java.util.List<? extends java.lang.Object> foo = dummyFixture.getList("label.greet");
			    logger.trace(" [test step] - Stop application");
			    dummyFixture.stopApplication();
			    logger.info(" [Test specification] * Do something different");
			    
			  }
			}
		'''.toString)
	}

}
