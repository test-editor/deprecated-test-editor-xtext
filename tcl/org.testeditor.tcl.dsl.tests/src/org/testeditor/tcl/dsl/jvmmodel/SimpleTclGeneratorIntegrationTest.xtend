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
				- assert foo                                  // null check 
				- assert baz = "fix"                          // assertEquals for String
				- assert book                                 // checked to be true (not (only) null check)
				- assert mak."key with spaces" = "fox"        // assertEquals with map dereferenced
				- assert ! foo								  // not null check
				- assert baz <> "fix"						  // assertNotEquals
				- assert ! book                               // checked to be false (not (only) null check)
				- assert mak."key with spaces" <> "fox"       // asssertNotEquals with map dereferenced
				- assert mak."key with spaces"                // null check with map dereferenced
				- assert ! mak."key with spaces"              // not null check with map dereferenced
				- assert baz = mak.otherkey                   // assert equals with map dereferenced
				- log = Read long from <bar>
				- assert log < "42"							  // parse 42 to long value and compare
				- assert log != baz                           // parse baz to long value and compare
				- assert baz != book                          // parse baz to boolean and compare
				- assert mak."key" < "42"                     // parse dereferenced map and 42 to long and compare
				- assert mak."key" = "42"                     // no parse, compare stringwise (since map access expects a string)
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
			  org.junit.Assert.assertNotNull("SimpleTest.tcl:11: foo", foo);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert baz = \"fix\"");
			  org.junit.Assert.assertEquals("SimpleTest.tcl:12: baz = \"fix\"", "fix", baz);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert book");
			  org.junit.Assert.assertTrue("SimpleTest.tcl:13: book", book);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key with spaces\" = \"fox\"");
			  org.junit.Assert.assertEquals("SimpleTest.tcl:14: mak.\"key with spaces\" = \"fox\"", "fox", mak.get("key with spaces"));
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert ! foo");
			  org.junit.Assert.assertNull("SimpleTest.tcl:15: ! foo", foo);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert baz <> \"fix\"");
			  org.junit.Assert.assertNotEquals("SimpleTest.tcl:16: baz <> \"fix\"", "fix", baz);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert ! book");
			  org.junit.Assert.assertFalse("SimpleTest.tcl:17: ! book", book);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key with spaces\" <> \"fox\"");
			  org.junit.Assert.assertNotEquals("SimpleTest.tcl:18: mak.\"key with spaces\" <> \"fox\"", "fox", mak.get("key with spaces"));
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key with spaces\"");
			  org.junit.Assert.assertNotNull("SimpleTest.tcl:19: mak.\"key with spaces\"", mak.get("key with spaces"));
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert ! mak.\"key with spaces\"");
			  org.junit.Assert.assertNull("SimpleTest.tcl:20: ! mak.\"key with spaces\"", mak.get("key with spaces"));
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert baz = mak.otherkey");
			  org.junit.Assert.assertEquals("SimpleTest.tcl:21: baz = mak.otherkey", mak.get("otherkey"), baz);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "long log = Read long from <bar>");
			  long log = dummyFixture.getLong("label.greet");
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert log < \"42\"");
			  org.junit.Assert.assertTrue("SimpleTest.tcl:23: log < \"42\"", log < Long.parseLong("42"));
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert log != baz");
			  org.junit.Assert.assertNotEquals("SimpleTest.tcl:24: log != baz", Long.parseLong(baz), log);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert baz != book");
			  org.junit.Assert.assertNotEquals("SimpleTest.tcl:25: baz != book", book, Boolean.valueOf(baz));
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key\" < \"42\"");
			  org.junit.Assert.assertTrue("SimpleTest.tcl:26: mak.\"key\" < \"42\"", Long.parseLong(mak.get("key")) < Long.parseLong("42"));
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key\" = \"42\"");
			  org.junit.Assert.assertEquals("SimpleTest.tcl:27: mak.\"key\" = \"42\"", "42", mak.get("key"));
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

	/**
	 * The AML template has a different order than the parameters in the fixture method.
	 *
	 * <pre>
	 * interaction type setValueReversed {
	 *    template = "Set value" ${value} "to" ${element}
	 *    method = DummyFixture.setValue(element, value)
	 * }
	 * </pre>
	 */
	@Test
	def void interactionCallWithValueAndLocatorInReverseOrder() {
		// given
		val tcl = '''
			* Some step
				Mask: GreetingApplication
				- Set value "theValue" to <Input>
		'''

		// when
		val generatedCode = tcl.parseAndGenerate

		// then
		generatedCode.assertContains('''
			reporter.enter(TestRunReporter.SemanticUnit.STEP, "Set value \"theValue\" to <Input>");
			dummyFixture.setValue("text.input", "theValue");
		'''.indent(2))
	}	

}
