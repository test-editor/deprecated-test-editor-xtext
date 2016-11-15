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
		// empty tcl as package and name are added by parseAndGenerate
		val tcl = ''

		// when
		val generatedCode = tcl.parseAndGenerate

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

		// when
		val generatedCode = tcl.parseAndGenerate

		// then
		generatedCode.assertContains('''
			@Test
			public void execute() throws Exception {
			  
			  reporter.enter(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "Test assertions in the famous greeting application");
			  
			  reporter.enter(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication");
			  
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "java.util.List<? extends java.lang.Object> foo = Read list from <bar>");
			  java.util.List<? extends java.lang.Object> foo = dummyFixture.getList("label.greet");
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "java.lang.String baz = Read value from <bar>");
			  java.lang.String baz = dummyFixture.getValue("label.greet");
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "boolean book = Read bool from <bar>");
			  boolean book = dummyFixture.getBool("label.greet");
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "java.util.Map<? extends java.lang.Object, ? extends java.lang.Object> mak = Read map from <bar>");
			  java.util.Map<? extends java.lang.Object, ? extends java.lang.Object> mak = dummyFixture.getMap("label.greet");
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert foo");
			  org.junit.Assert.assertNotNull("foo", foo);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert baz = \"fix\"");
			  org.junit.Assert.assertEquals("baz = \"fix\"", "fix", baz);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert book");
			  org.junit.Assert.assertTrue("book", book);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key with spaces\" = \"fox\"");
			  org.junit.Assert.assertEquals("mak.\"key with spaces\" = \"fox\"", "fox", mak.get("key with spaces"));
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert ! foo");
			  org.junit.Assert.assertNull("! foo", foo);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert baz <> \"fix\"");
			  org.junit.Assert.assertNotEquals("baz <> \"fix\"", "fix", baz);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert ! book");
			  org.junit.Assert.assertFalse("! book", book);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key with spaces\" <> \"fox\"");
			  org.junit.Assert.assertNotEquals("mak.\"key with spaces\" <> \"fox\"", "fox", mak.get("key with spaces"));
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key with spaces\"");
			  org.junit.Assert.assertNotNull("mak.\"key with spaces\"", mak.get("key with spaces"));
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert ! mak.\"key with spaces\"");
			  org.junit.Assert.assertNull("! mak.\"key with spaces\"", mak.get("key with spaces"));
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert baz = mak.otherkey");
			  org.junit.Assert.assertEquals("baz = mak.otherkey", mak.get("otherkey"), baz);
			}
		'''.indent(1))
	}

	@Test
	def void testDefaultGeneration() {
		// given
		val tcl = '''
			* Start the famous greetings application
				Mask: GreetingApplication
				- Start application "org.testeditor.swing.exammple.Greetings"
				- foo = Read list from <bar>
				- Stop application
			
			* Do something different
		'''

		// when
		val generatedCode = tcl.parseAndGenerate

		// then
		generatedCode.assertContains('''
			@Test
			public void execute() throws Exception {
			  
			  reporter.enter(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "Start the famous greetings application");
			  
			  reporter.enter(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication");
			  
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "Start application \"org.testeditor.swing.exammple.Greetings\"");
			  dummyFixture.startApplication("org.testeditor.swing.exammple.Greetings");
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "java.util.List<? extends java.lang.Object> foo = Read list from <bar>");
			  java.util.List<? extends java.lang.Object> foo = dummyFixture.getList("label.greet");
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "Stop application");
			  dummyFixture.stopApplication();
			  reporter.enter(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "Do something different");
			  
			}
		'''.indent(1))
	}

	@Test
	def void testStepWithQuestionMark() {
		// given
		val tcl = '''
			* Start the famous greetings application
				Mask: GreetingApplication
				- Is <bar> visible?
		'''

		// when
		val generatedCode = tcl.parseAndGenerate

		// then
		generatedCode.assertContains('''
			reporter.enter(TestRunReporter.SemanticUnit.STEP, "Is <bar> visible?");
			dummyFixture.isVisible("label.greet");
		'''.indent(2))
	}

	@Test
	def void interactionCallWithValueAndLocator() {
		// given
		val tcl = '''
			* Some step
				Mask: GreetingApplication
				- Set value of <Input> to "theValue"
		'''

		// when
		val generatedCode = tcl.parseAndGenerate

		// then
		generatedCode.assertContains('''
			reporter.enter(TestRunReporter.SemanticUnit.STEP, "Set value of <Input> to \"theValue\"");
			dummyFixture.setValue("text.input", "theValue");
		'''.indent(2))
	}

}
