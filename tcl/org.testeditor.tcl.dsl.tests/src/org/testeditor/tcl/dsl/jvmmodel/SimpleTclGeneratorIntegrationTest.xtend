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
	def void parseMacroModel() {
		parseTcl(DummyFixture.getMacroModel("GreetingApplicationMacro"), "GreetingApplicationMacro.tml")
	}

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
				- mak = Read jsonObject from <bar>
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
				- assert log < 42							  // parse 42 to long value and compare
				- assert log != baz                           // parse baz to long value and compare
				- assert baz != book                          // convert book to string and compare
				- assert mak."key" < "42"                     // parse dereferenced map and 42 to long and compare
				- assert mak."key" < 42                       // parse dereferenced map and 42 to long and compare
				- assert mak."key" = "42"                     // no parse, compare stringwise (since map access expects a string)
				- assert mak."key" = 42                       // compare values
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
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "com.google.gson.JsonObject mak = Read jsonObject from <bar>");
			  com.google.gson.JsonObject mak = dummyFixture.getJsonObject("label.greet");
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert foo");
			  org.junit.Assert.assertNotNull("SimpleTest.tcl:11: foo", foo);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert baz = \"fix\"");
			  org.junit.Assert.assertEquals("SimpleTest.tcl:12: baz = \"fix\"", "fix", baz);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert book");
			  org.junit.Assert.assertTrue("SimpleTest.tcl:13: book", book);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key with spaces\" = \"fox\"");
			  org.junit.Assert.assertEquals("SimpleTest.tcl:14: mak.\"key with spaces\" = \"fox\"", "fox", mak.getAsJsonObject().get("key with spaces").getAsJsonPrimitive().getAsString());
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert ! foo");
			  org.junit.Assert.assertNull("SimpleTest.tcl:15: ! foo", foo);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert baz <> \"fix\"");
			  org.junit.Assert.assertNotEquals("SimpleTest.tcl:16: baz <> \"fix\"", "fix", baz);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert ! book");
			  org.junit.Assert.assertFalse("SimpleTest.tcl:17: ! book", book);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key with spaces\" <> \"fox\"");
			  org.junit.Assert.assertNotEquals("SimpleTest.tcl:18: mak.\"key with spaces\" <> \"fox\"", "fox", mak.getAsJsonObject().get("key with spaces").getAsJsonPrimitive().getAsString());
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key with spaces\"");
			  org.junit.Assert.assertNotNull("SimpleTest.tcl:19: mak.\"key with spaces\"", mak.getAsJsonObject().get("key with spaces"));
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert ! mak.\"key with spaces\"");
			  org.junit.Assert.assertNull("SimpleTest.tcl:20: ! mak.\"key with spaces\"", mak.getAsJsonObject().get("key with spaces"));
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert baz = mak.otherkey");
			  org.junit.Assert.assertEquals("SimpleTest.tcl:21: baz = mak.otherkey", mak.getAsJsonObject().get("otherkey").getAsJsonPrimitive().getAsString(), baz);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "long log = Read long from <bar>");
			  long log = dummyFixture.getLong("label.greet");
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert log < 42");
			  org.junit.Assert.assertTrue("SimpleTest.tcl:23: log < 42", log < 42);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert log != baz");
			  org.junit.Assert.assertNotEquals("SimpleTest.tcl:24: log != baz", Long.parseLong(baz), log);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert baz != book");
			  org.junit.Assert.assertNotEquals("SimpleTest.tcl:25: baz != book", Boolean.toString(book), baz);
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key\" < \"42\"");
			  org.junit.Assert.assertTrue("SimpleTest.tcl:26: mak.\"key\" < \"42\"", mak.getAsJsonObject().get("key").getAsJsonPrimitive().getAsBigDecimal() < new java.math.BigDecimal("42"));
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key\" < 42");
			  org.junit.Assert.assertTrue("SimpleTest.tcl:27: mak.\"key\" < 42", mak.getAsJsonObject().get("key").getAsJsonPrimitive().getAsBigDecimal() < new java.math.BigDecimal(42));
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key\" = \"42\"");
			  org.junit.Assert.assertEquals("SimpleTest.tcl:28: mak.\"key\" = \"42\"", "42", mak.getAsJsonObject().get("key").getAsJsonPrimitive().getAsString());
			  reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key\" = 42");
			  org.junit.Assert.assertEquals("SimpleTest.tcl:29: mak.\"key\" = 42", 42, mak.getAsJsonObject().get("key").getAsJsonPrimitive().getAsNumber());
			}
		'''.indent(1))
	}
	
	@Test
	def void testCoercionAndChecks() {
		// given
		val tcl = '''
			* Test assertions in the famous greeting application
				Mask: GreetingApplication
				// read variables of several types
				- jsonvar = Read jsonObject from <bar>
				- longvar = Read long from <bar>
				- stringvar = Read value from <bar>
				- boolvar = Read bool from <bar>
				
				// usage of several types in map assignment
				- jsonvar.key = jsonvar."some value"
				- jsonvar."other key" = "value"
				- jsonvar.key2 = stringvar // NO conversion, parsing is done
				- jsonvar.key3 = longvar // NO conversion, parsing is done 
				- jsonvar.key4 = boolvar // NO conversion, parsing is done
				
				// usage of several types in method calls
				- Type boolean @stringvar into <Input> // conversion && check from String to boolean
				- TypeLong @stringvar into <Input> // converion && check from String to long
				- Type boolean @jsonvar.key5 into <Input> // conversion && check from String to boolean
				- TypeLong @jsonvar.key6 into <Input> // converion && check from String to long
			
				Macro: GreetingApplicationMacro
				// usage of several types in macro calls
				- TypeBoolean @jsonvar.key into input field // conversion && check
				- TypeLong @jsonvar."key" into input field // conversion && check from String to long
				- TypeBoolean @stringvar into input field // conversion && check from String to boolean
				- TypeLong @stringvar into input field // converion && check from String to long
		'''

		// when
		val generatedCode = tcl.parseAndGenerate

		// then
		generatedCode.assertContains('''
			reporter.enter(TestRunReporter.SemanticUnit.STEP, "com.google.gson.JsonObject jsonvar = Read jsonObject from <bar>");
			com.google.gson.JsonObject jsonvar = dummyFixture.getJsonObject("label.greet");
			reporter.enter(TestRunReporter.SemanticUnit.STEP, "long longvar = Read long from <bar>");
			long longvar = dummyFixture.getLong("label.greet");
			reporter.enter(TestRunReporter.SemanticUnit.STEP, "java.lang.String stringvar = Read value from <bar>");
			java.lang.String stringvar = dummyFixture.getValue("label.greet");
			reporter.enter(TestRunReporter.SemanticUnit.STEP, "boolean boolvar = Read bool from <bar>");
			boolean boolvar = dummyFixture.getBool("label.greet");
			reporter.enter(TestRunReporter.SemanticUnit.STEP, "@jsonvar.\"key\" = jsonvar.\"some value\"");
			jsonvar.getAsJsonObject().add("key", jsonvar.getAsJsonObject().get("some value"));
			reporter.enter(TestRunReporter.SemanticUnit.STEP, "@jsonvar.\"other key\" = \"value\"");
			jsonvar.getAsJsonObject().add("other key", new com.google.gson.JsonParser().parse("\"value\""));
			reporter.enter(TestRunReporter.SemanticUnit.STEP, "@jsonvar.\"key2\" = stringvar");
			jsonvar.getAsJsonObject().add("key2", new com.google.gson.JsonParser().parse("\""+stringvar+"\""));
			reporter.enter(TestRunReporter.SemanticUnit.STEP, "@jsonvar.\"key3\" = longvar");
			jsonvar.getAsJsonObject().add("key3", new com.google.gson.JsonParser().parse(Long.toString(longvar)));
			reporter.enter(TestRunReporter.SemanticUnit.STEP, "@jsonvar.\"key4\" = boolvar");
			jsonvar.getAsJsonObject().add("key4", new com.google.gson.JsonParser().parse(Boolean.toString(boolvar)));
			reporter.enter(TestRunReporter.SemanticUnit.STEP, "Type boolean @stringvar into <Input>");
			org.junit.Assert.assertTrue("Parameter is expected to be of type = 'boolean' but a non coercible value = '"+stringvar.toString()+"' was passed through variable reference = 'stringvar'.", Boolean.TRUE.toString().equals(stringvar) || Boolean.FALSE.toString().equals(stringvar));
			dummyFixture.typeBoolInto("text.input", Boolean.valueOf(stringvar));
			reporter.enter(TestRunReporter.SemanticUnit.STEP, "TypeLong @stringvar into <Input>");
			try { Long.parseLong(stringvar); } catch (NumberFormatException nfe) { org.junit.Assert.fail("Parameter is expected to be of type = 'long' but a non coercible value = '"+stringvar.toString()+"' was passed through variable reference = 'stringvar'."); }
			dummyFixture.typeLongInto("text.input", org.testeditor.dsl.common.testing.DummyLocatorStrategy.ID, Long.parseLong(stringvar));
			reporter.enter(TestRunReporter.SemanticUnit.STEP, "Type boolean @jsonvar.\"key5\" into <Input>");
			org.junit.Assert.assertTrue("Parameter is expected to be of type = 'boolean' but a non coercible value = '"+jsonvar.getAsJsonObject().get("key5").toString()+"' was passed through variable reference = 'jsonvar'.", jsonvar.getAsJsonObject().get("key5").getAsJsonPrimitive().isBoolean());
			dummyFixture.typeBoolInto("text.input", jsonvar.getAsJsonObject().get("key5").getAsJsonPrimitive().getAsBoolean());
			reporter.enter(TestRunReporter.SemanticUnit.STEP, "TypeLong @jsonvar.\"key6\" into <Input>");
			org.junit.Assert.assertTrue("Parameter is expected to be of type = 'long' but a non coercible value = '"+jsonvar.getAsJsonObject().get("key6").toString()+"' was passed through variable reference = 'jsonvar'.", jsonvar.getAsJsonObject().get("key6").getAsJsonPrimitive().isNumber());
			dummyFixture.typeLongInto("text.input", org.testeditor.dsl.common.testing.DummyLocatorStrategy.ID, jsonvar.getAsJsonObject().get("key6").getAsJsonPrimitive().getAsLong());
			// Macro: GreetingApplicationMacro
			// - TypeBoolean @jsonvar."key" into input field
			org.junit.Assert.assertTrue("Parameter is expected to be of type = 'boolean' but a non coercible value = '"+jsonvar.getAsJsonObject().get("key").toString()+"' was passed through variable reference = 'jsonvar'.", jsonvar.getAsJsonObject().get("key").getAsJsonPrimitive().isBoolean());
			macro_GreetingApplicationMacro_TypeBoolIntoInputField(jsonvar.getAsJsonObject().get("key").getAsJsonPrimitive().getAsBoolean());
			// - TypeLong @jsonvar."key" into input field
			org.junit.Assert.assertTrue("Parameter is expected to be of type = 'long' but a non coercible value = '"+jsonvar.getAsJsonObject().get("key").toString()+"' was passed through variable reference = 'jsonvar'.", jsonvar.getAsJsonObject().get("key").getAsJsonPrimitive().isNumber());
			macro_GreetingApplicationMacro_TypeLongIntoInputField(jsonvar.getAsJsonObject().get("key").getAsJsonPrimitive().getAsLong());
			// - TypeBoolean @stringvar into input field
			org.junit.Assert.assertTrue("Parameter is expected to be of type = 'boolean' but a non coercible value = '"+stringvar.toString()+"' was passed through variable reference = 'stringvar'.", Boolean.TRUE.toString().equals(stringvar) || Boolean.FALSE.toString().equals(stringvar));
			macro_GreetingApplicationMacro_TypeBoolIntoInputField(Boolean.valueOf(stringvar));
			// - TypeLong @stringvar into input field
			try { Long.parseLong(stringvar); } catch (NumberFormatException nfe) { org.junit.Assert.fail("Parameter is expected to be of type = 'long' but a non coercible value = '"+stringvar.toString()+"' was passed through variable reference = 'stringvar'."); }
			macro_GreetingApplicationMacro_TypeLongIntoInputField(Long.parseLong(stringvar));
		'''.indent(2))
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
