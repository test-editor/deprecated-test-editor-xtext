/*******************************************************************************
 * Copyright (c) 2012 - 2018 Signal Iduna Corporation and others.
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
		parseTcl(DummyFixture.getMacroModel("GreetingApplicationMacro"))
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
			import org.testeditor.fixture.core.TestRunReporter;
			
			/**
			 * Generated from SimpleTest.tcl
			 */
			@SuppressWarnings("all")
			public class SimpleTest extends AbstractTestCase {
			  @Test
			  public void execute() throws Exception {
			    try {
			      finishedTestWith(TestRunReporter.Status.OK); // reaching this line of code means successful test execution
			    } catch (AssertionError e) {
			      reporter.assertionExit(e);
			      finishedTestWith(TestRunReporter.Status.ERROR);
			      org.junit.Assert.fail(e.getMessage());
			    } catch (Exception e) {
			      reporter.exceptionExit(e);
			      finishedTestWith(TestRunReporter.Status.ABORTED);
			      org.junit.Assert.fail(e.getMessage());
			    }
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
				- assert log < 42                             // parse 42 to long value and compare
				- assert log != baz                           // parse baz to long value and compare
				- assert baz != book                          // convert book to string and compare
				- assert mak."key" < "42"                     // parse dereferenced map and 42 to bigDecimal and compare
				- assert mak."key" < 42                       // parse dereferenced map and 42 to bigDecimal and compare
				- assert mak."key" = "42"                     // no parse, compare stringwise (since map access expects a string)
				- assert mak."key" = 42                       // compare values
		'''

		// when
		val generatedCode = tcl.parseAndGenerate

		// then
		generatedCode.assertContains('''
			  @Test
			  public void execute() throws Exception {
			    try {
			      String IDvar0=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "Test assertions in the famous greeting application", IDvar0, TestRunReporter.Status.STARTED, variables());
			      String IDvar1=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication", IDvar1, TestRunReporter.Status.STARTED, variables());
			      String IDvar2=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "foo = Read list from <bar> [java.util.List<? extends java.lang.Object>]", IDvar2, TestRunReporter.Status.STARTED, variables());
			      java.util.List<? extends java.lang.Object> foo = dummyFixture.getList("label.greet");
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "foo = Read list from <bar> [java.util.List<? extends java.lang.Object>]", IDvar2, TestRunReporter.Status.OK, variables());
			      String IDvar3=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "baz = Read value from <bar> [java.lang.String]", IDvar3, TestRunReporter.Status.STARTED, variables());
			      java.lang.String baz = dummyFixture.getValue("label.greet");
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "baz = Read value from <bar> [java.lang.String]", IDvar3, TestRunReporter.Status.OK, variables());
			      String IDvar4=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "book = Read bool from <bar> [boolean]", IDvar4, TestRunReporter.Status.STARTED, variables());
			      boolean book = dummyFixture.getBool("label.greet");
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "book = Read bool from <bar> [boolean]", IDvar4, TestRunReporter.Status.OK, variables());
			      String IDvar5=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "mak = Read jsonObject from <bar> [com.google.gson.JsonObject]", IDvar5, TestRunReporter.Status.STARTED, variables());
			      com.google.gson.JsonObject mak = dummyFixture.getJsonObject("label.greet");
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "mak = Read jsonObject from <bar> [com.google.gson.JsonObject]", IDvar5, TestRunReporter.Status.OK, variables());
			      String IDvar6=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert foo", IDvar6, TestRunReporter.Status.STARTED, variables("foo", foo.toString()));
			      org.junit.Assert.assertNotNull("SimpleTest.tcl:11: foo", foo);
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "assert foo", IDvar6, TestRunReporter.Status.OK, variables("foo", foo.toString()));
			      String IDvar7=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert baz = \"fix\"", IDvar7, TestRunReporter.Status.STARTED, variables("baz", baz));
			      org.junit.Assert.assertEquals("SimpleTest.tcl:12: baz = \"fix\"", "fix", baz);
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "assert baz = \"fix\"", IDvar7, TestRunReporter.Status.OK, variables("baz", baz));
			      String IDvar8=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert book", IDvar8, TestRunReporter.Status.STARTED, variables("book", Boolean.toString(book)));
			      org.junit.Assert.assertTrue("SimpleTest.tcl:13: book", book);
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "assert book", IDvar8, TestRunReporter.Status.OK, variables("book", Boolean.toString(book)));
			      String IDvar9=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key with spaces\" = \"fox\"", IDvar9, TestRunReporter.Status.STARTED, variables("mak.\"key with spaces\"", mak.getAsJsonObject().get("key with spaces").getAsJsonPrimitive().getAsString()));
			      org.junit.Assert.assertEquals("SimpleTest.tcl:14: mak.\"key with spaces\" = \"fox\"", "fox", mak.getAsJsonObject().get("key with spaces").getAsJsonPrimitive().getAsString());
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key with spaces\" = \"fox\"", IDvar9, TestRunReporter.Status.OK, variables("mak.\"key with spaces\"", mak.getAsJsonObject().get("key with spaces").getAsJsonPrimitive().getAsString()));
			      String IDvar10=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert ! foo", IDvar10, TestRunReporter.Status.STARTED, variables("foo", foo.toString()));
			      org.junit.Assert.assertNull("SimpleTest.tcl:15: ! foo", foo);
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "assert ! foo", IDvar10, TestRunReporter.Status.OK, variables("foo", foo.toString()));
			      String IDvar11=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert baz <> \"fix\"", IDvar11, TestRunReporter.Status.STARTED, variables("baz", baz));
			      org.junit.Assert.assertNotEquals("SimpleTest.tcl:16: baz <> \"fix\"", "fix", baz);
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "assert baz <> \"fix\"", IDvar11, TestRunReporter.Status.OK, variables("baz", baz));
			      String IDvar12=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert ! book", IDvar12, TestRunReporter.Status.STARTED, variables("book", Boolean.toString(book)));
			      org.junit.Assert.assertFalse("SimpleTest.tcl:17: ! book", book);
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "assert ! book", IDvar12, TestRunReporter.Status.OK, variables("book", Boolean.toString(book)));
			      String IDvar13=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key with spaces\" <> \"fox\"", IDvar13, TestRunReporter.Status.STARTED, variables("mak.\"key with spaces\"", mak.getAsJsonObject().get("key with spaces").getAsJsonPrimitive().getAsString()));
			      org.junit.Assert.assertNotEquals("SimpleTest.tcl:18: mak.\"key with spaces\" <> \"fox\"", "fox", mak.getAsJsonObject().get("key with spaces").getAsJsonPrimitive().getAsString());
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key with spaces\" <> \"fox\"", IDvar13, TestRunReporter.Status.OK, variables("mak.\"key with spaces\"", mak.getAsJsonObject().get("key with spaces").getAsJsonPrimitive().getAsString()));
			      String IDvar14=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key with spaces\"", IDvar14, TestRunReporter.Status.STARTED, variables("mak.\"key with spaces\"", mak.getAsJsonObject().get("key with spaces").getAsJsonPrimitive().getAsString()));
			      org.junit.Assert.assertNotNull("SimpleTest.tcl:19: mak.\"key with spaces\"", mak.getAsJsonObject().get("key with spaces"));
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key with spaces\"", IDvar14, TestRunReporter.Status.OK, variables("mak.\"key with spaces\"", mak.getAsJsonObject().get("key with spaces").getAsJsonPrimitive().getAsString()));
			      String IDvar15=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert ! mak.\"key with spaces\"", IDvar15, TestRunReporter.Status.STARTED, variables("mak.\"key with spaces\"", mak.getAsJsonObject().get("key with spaces").getAsJsonPrimitive().getAsString()));
			      org.junit.Assert.assertNull("SimpleTest.tcl:20: ! mak.\"key with spaces\"", mak.getAsJsonObject().get("key with spaces"));
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "assert ! mak.\"key with spaces\"", IDvar15, TestRunReporter.Status.OK, variables("mak.\"key with spaces\"", mak.getAsJsonObject().get("key with spaces").getAsJsonPrimitive().getAsString()));
			      String IDvar16=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert baz = mak.otherkey", IDvar16, TestRunReporter.Status.STARTED, variables("baz", baz, "mak.\"otherkey\"", mak.getAsJsonObject().get("otherkey").getAsJsonPrimitive().getAsString()));
			      org.junit.Assert.assertEquals("SimpleTest.tcl:21: baz = mak.otherkey", mak.getAsJsonObject().get("otherkey").getAsJsonPrimitive().getAsString(), baz);
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "assert baz = mak.otherkey", IDvar16, TestRunReporter.Status.OK, variables("baz", baz, "mak.\"otherkey\"", mak.getAsJsonObject().get("otherkey").getAsJsonPrimitive().getAsString()));
			      String IDvar17=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "log = Read long from <bar> [long]", IDvar17, TestRunReporter.Status.STARTED, variables());
			      long log = dummyFixture.getLong("label.greet");
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "log = Read long from <bar> [long]", IDvar17, TestRunReporter.Status.OK, variables());
			      String IDvar18=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert log < 42", IDvar18, TestRunReporter.Status.STARTED, variables("log", Long.toString(log)));
			      org.junit.Assert.assertTrue("SimpleTest.tcl:23: log < 42", log < 42);
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "assert log < 42", IDvar18, TestRunReporter.Status.OK, variables("log", Long.toString(log)));
			      String IDvar19=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert log != baz", IDvar19, TestRunReporter.Status.STARTED, variables("log", Long.toString(log), "baz", baz));
			      org.junit.Assert.assertNotEquals("SimpleTest.tcl:24: log != baz", Long.parseLong(baz), log);
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "assert log != baz", IDvar19, TestRunReporter.Status.OK, variables("log", Long.toString(log), "baz", baz));
			      String IDvar20=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert baz != book", IDvar20, TestRunReporter.Status.STARTED, variables("baz", baz, "book", Boolean.toString(book)));
			      org.junit.Assert.assertNotEquals("SimpleTest.tcl:25: baz != book", Boolean.toString(book), baz);
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "assert baz != book", IDvar20, TestRunReporter.Status.OK, variables("baz", baz, "book", Boolean.toString(book)));
			      String IDvar21=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key\" < \"42\"", IDvar21, TestRunReporter.Status.STARTED, variables("mak.\"key\"", mak.getAsJsonObject().get("key").getAsJsonPrimitive().getAsString()));
			      org.junit.Assert.assertTrue("SimpleTest.tcl:26: mak.\"key\" < \"42\"", mak.getAsJsonObject().get("key").getAsJsonPrimitive().getAsBigDecimal().compareTo(new java.math.BigDecimal("42")) < 0);
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key\" < \"42\"", IDvar21, TestRunReporter.Status.OK, variables("mak.\"key\"", mak.getAsJsonObject().get("key").getAsJsonPrimitive().getAsString()));
			      String IDvar22=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key\" < 42", IDvar22, TestRunReporter.Status.STARTED, variables("mak.\"key\"", mak.getAsJsonObject().get("key").getAsJsonPrimitive().getAsString()));
			      org.junit.Assert.assertTrue("SimpleTest.tcl:27: mak.\"key\" < 42", mak.getAsJsonObject().get("key").getAsJsonPrimitive().getAsBigDecimal().compareTo(new java.math.BigDecimal(42)) < 0);
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key\" < 42", IDvar22, TestRunReporter.Status.OK, variables("mak.\"key\"", mak.getAsJsonObject().get("key").getAsJsonPrimitive().getAsString()));
			      String IDvar23=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key\" = \"42\"", IDvar23, TestRunReporter.Status.STARTED, variables("mak.\"key\"", mak.getAsJsonObject().get("key").getAsJsonPrimitive().getAsString()));
			      org.junit.Assert.assertEquals("SimpleTest.tcl:28: mak.\"key\" = \"42\"", "42", mak.getAsJsonObject().get("key").getAsJsonPrimitive().getAsString());
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key\" = \"42\"", IDvar23, TestRunReporter.Status.OK, variables("mak.\"key\"", mak.getAsJsonObject().get("key").getAsJsonPrimitive().getAsString()));
			      String IDvar24=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key\" = 42", IDvar24, TestRunReporter.Status.STARTED, variables("mak.\"key\"", mak.getAsJsonObject().get("key").getAsJsonPrimitive().getAsString()));
			      org.junit.Assert.assertEquals("SimpleTest.tcl:29: mak.\"key\" = 42", 42, mak.getAsJsonObject().get("key").getAsJsonPrimitive().getAsNumber());
			      reporter.leave(TestRunReporter.SemanticUnit.STEP, "assert mak.\"key\" = 42", IDvar24, TestRunReporter.Status.OK, variables("mak.\"key\"", mak.getAsJsonObject().get("key").getAsJsonPrimitive().getAsString()));
			      reporter.leave(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication", IDvar1, TestRunReporter.Status.OK, variables());
			      reporter.leave(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "Test assertions in the famous greeting application", IDvar0, TestRunReporter.Status.OK, variables());
			      finishedTestWith(TestRunReporter.Status.OK); // reaching this line of code means successful test execution
			    } catch (AssertionError e) {
			      reporter.assertionExit(e);
			      finishedTestWith(TestRunReporter.Status.ERROR);
			      org.junit.Assert.fail(e.getMessage());
			    } catch (org.testeditor.fixture.core.FixtureException e) {
			      reporter.fixtureExit(e);
			      finishedTestWith(TestRunReporter.Status.ABORTED);
			      org.junit.Assert.fail(e.getMessage());
			    } catch (Exception e) {
			      reporter.exceptionExit(e);
			      finishedTestWith(TestRunReporter.Status.ABORTED);
			      org.junit.Assert.fail(e.getMessage());
			    }
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
				- enumvar = Read enum from <bar>
				
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
				- Set enum of <Input> to @enumvar
				- Set enum of <Input> to @stringvar
				- Set enum of <Input> to "enum_a"
			
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
			String IDvar2=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "jsonvar = Read jsonObject from <bar> [com.google.gson.JsonObject]", IDvar2, TestRunReporter.Status.STARTED, variables());
			com.google.gson.JsonObject jsonvar = dummyFixture.getJsonObject("label.greet");
			reporter.leave(TestRunReporter.SemanticUnit.STEP, "jsonvar = Read jsonObject from <bar> [com.google.gson.JsonObject]", IDvar2, TestRunReporter.Status.OK, variables());
			String IDvar3=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "longvar = Read long from <bar> [long]", IDvar3, TestRunReporter.Status.STARTED, variables());
			long longvar = dummyFixture.getLong("label.greet");
			reporter.leave(TestRunReporter.SemanticUnit.STEP, "longvar = Read long from <bar> [long]", IDvar3, TestRunReporter.Status.OK, variables());
			String IDvar4=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "stringvar = Read value from <bar> [java.lang.String]", IDvar4, TestRunReporter.Status.STARTED, variables());
			java.lang.String stringvar = dummyFixture.getValue("label.greet");
			reporter.leave(TestRunReporter.SemanticUnit.STEP, "stringvar = Read value from <bar> [java.lang.String]", IDvar4, TestRunReporter.Status.OK, variables());
			String IDvar5=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "boolvar = Read bool from <bar> [boolean]", IDvar5, TestRunReporter.Status.STARTED, variables());
			boolean boolvar = dummyFixture.getBool("label.greet");
			reporter.leave(TestRunReporter.SemanticUnit.STEP, "boolvar = Read bool from <bar> [boolean]", IDvar5, TestRunReporter.Status.OK, variables());
			String IDvar6=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "enumvar = Read enum from <bar> [org.testeditor.dsl.common.testing.DummyEnum]", IDvar6, TestRunReporter.Status.STARTED, variables());
			org.testeditor.dsl.common.testing.DummyEnum enumvar = dummyFixture.getEnum("label.greet");
			reporter.leave(TestRunReporter.SemanticUnit.STEP, "enumvar = Read enum from <bar> [org.testeditor.dsl.common.testing.DummyEnum]", IDvar6, TestRunReporter.Status.OK, variables());
			String IDvar7=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "jsonvar.\"key\" = jsonvar.\"some value\"", IDvar7, TestRunReporter.Status.STARTED, variables());
			jsonvar.getAsJsonObject().add("key", jsonvar.getAsJsonObject().get("some value"));
			reporter.leave(TestRunReporter.SemanticUnit.STEP, "jsonvar.\"key\" = jsonvar.\"some value\"", IDvar7, TestRunReporter.Status.OK, variables());
			String IDvar8=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "jsonvar.\"other key\" = \"value\"", IDvar8, TestRunReporter.Status.STARTED, variables());
			jsonvar.getAsJsonObject().add("other key", new com.google.gson.JsonParser().parse("\"value\""));
			reporter.leave(TestRunReporter.SemanticUnit.STEP, "jsonvar.\"other key\" = \"value\"", IDvar8, TestRunReporter.Status.OK, variables());
			String IDvar9=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "jsonvar.\"key2\" = stringvar", IDvar9, TestRunReporter.Status.STARTED, variables());
			jsonvar.getAsJsonObject().add("key2", new com.google.gson.JsonParser().parse("\""+stringvar+"\""));
			reporter.leave(TestRunReporter.SemanticUnit.STEP, "jsonvar.\"key2\" = stringvar", IDvar9, TestRunReporter.Status.OK, variables());
			String IDvar10=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "jsonvar.\"key3\" = longvar", IDvar10, TestRunReporter.Status.STARTED, variables());
			jsonvar.getAsJsonObject().add("key3", new com.google.gson.JsonParser().parse(Long.toString(longvar)));
			reporter.leave(TestRunReporter.SemanticUnit.STEP, "jsonvar.\"key3\" = longvar", IDvar10, TestRunReporter.Status.OK, variables());
			String IDvar11=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "jsonvar.\"key4\" = boolvar", IDvar11, TestRunReporter.Status.STARTED, variables());
			jsonvar.getAsJsonObject().add("key4", new com.google.gson.JsonParser().parse(Boolean.toString(boolvar)));
			reporter.leave(TestRunReporter.SemanticUnit.STEP, "jsonvar.\"key4\" = boolvar", IDvar11, TestRunReporter.Status.OK, variables());
			String IDvar12=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Type boolean @stringvar into <Input>", IDvar12, TestRunReporter.Status.STARTED, variables("stringvar", stringvar));
			org.junit.Assert.assertTrue("Parameter is expected to be of type = 'boolean' but a non coercible value = '"+stringvar.toString()+"' was passed through variable reference = 'stringvar'.", Boolean.TRUE.toString().equals(stringvar) || Boolean.FALSE.toString().equals(stringvar));
			dummyFixture.typeBoolInto("text.input", Boolean.valueOf(stringvar));
			reporter.leave(TestRunReporter.SemanticUnit.STEP, "Type boolean @stringvar into <Input>", IDvar12, TestRunReporter.Status.OK, variables("stringvar", stringvar));
			String IDvar13=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "TypeLong @stringvar into <Input>", IDvar13, TestRunReporter.Status.STARTED, variables("stringvar", stringvar));
			try { Long.parseLong(stringvar); } catch (NumberFormatException nfe) { org.junit.Assert.fail("Parameter is expected to be of type = 'long' but a non coercible value = '"+stringvar.toString()+"' was passed through variable reference = 'stringvar'."); }
			dummyFixture.typeLongInto("text.input", org.testeditor.dsl.common.testing.DummyLocatorStrategy.ID, Long.parseLong(stringvar));
			reporter.leave(TestRunReporter.SemanticUnit.STEP, "TypeLong @stringvar into <Input>", IDvar13, TestRunReporter.Status.OK, variables("stringvar", stringvar));
			String IDvar14=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Type boolean @jsonvar.\"key5\" into <Input>", IDvar14, TestRunReporter.Status.STARTED, variables("jsonvar.\"key5\"", jsonvar.getAsJsonObject().get("key5").getAsJsonPrimitive().getAsString()));
			org.junit.Assert.assertTrue("Parameter is expected to be of type = 'boolean' but a non coercible value = '"+jsonvar.getAsJsonObject().get("key5").toString()+"' was passed through variable reference = 'jsonvar'.", jsonvar.getAsJsonObject().get("key5").getAsJsonPrimitive().isBoolean());
			dummyFixture.typeBoolInto("text.input", jsonvar.getAsJsonObject().get("key5").getAsJsonPrimitive().getAsBoolean());
			reporter.leave(TestRunReporter.SemanticUnit.STEP, "Type boolean @jsonvar.\"key5\" into <Input>", IDvar14, TestRunReporter.Status.OK, variables("jsonvar.\"key5\"", jsonvar.getAsJsonObject().get("key5").getAsJsonPrimitive().getAsString()));
			String IDvar15=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "TypeLong @jsonvar.\"key6\" into <Input>", IDvar15, TestRunReporter.Status.STARTED, variables("jsonvar.\"key6\"", jsonvar.getAsJsonObject().get("key6").getAsJsonPrimitive().getAsString()));
			org.junit.Assert.assertTrue("Parameter is expected to be of type = 'long' but a non coercible value = '"+jsonvar.getAsJsonObject().get("key6").toString()+"' was passed through variable reference = 'jsonvar'.", jsonvar.getAsJsonObject().get("key6").getAsJsonPrimitive().isNumber());
			dummyFixture.typeLongInto("text.input", org.testeditor.dsl.common.testing.DummyLocatorStrategy.ID, jsonvar.getAsJsonObject().get("key6").getAsJsonPrimitive().getAsLong());
			reporter.leave(TestRunReporter.SemanticUnit.STEP, "TypeLong @jsonvar.\"key6\" into <Input>", IDvar15, TestRunReporter.Status.OK, variables("jsonvar.\"key6\"", jsonvar.getAsJsonObject().get("key6").getAsJsonPrimitive().getAsString()));
			String IDvar16=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Set enum of <Input> to @enumvar", IDvar16, TestRunReporter.Status.STARTED, variables("enumvar", enumvar.toString()));
			dummyFixture.setEnum("text.input", enumvar);
			reporter.leave(TestRunReporter.SemanticUnit.STEP, "Set enum of <Input> to @enumvar", IDvar16, TestRunReporter.Status.OK, variables("enumvar", enumvar.toString()));
			String IDvar17=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Set enum of <Input> to @stringvar", IDvar17, TestRunReporter.Status.STARTED, variables("stringvar", stringvar));
			try { org.testeditor.dsl.common.testing.DummyEnum.valueOf(stringvar); } catch (IllegalArgumentException ia) { org.junit.Assert.fail("Parameter is expected to be of type = 'org.testeditor.dsl.common.testing.DummyEnum' but a non coercible value = '"+stringvar.toString()+"' was passed through variable reference = 'stringvar'."); }
			dummyFixture.setEnum("text.input", org.testeditor.dsl.common.testing.DummyEnum.valueOf(stringvar));
			reporter.leave(TestRunReporter.SemanticUnit.STEP, "Set enum of <Input> to @stringvar", IDvar17, TestRunReporter.Status.OK, variables("stringvar", stringvar));
			String IDvar18=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Set enum of <Input> to \"enum_a\"", IDvar18, TestRunReporter.Status.STARTED, variables());
			dummyFixture.setEnum("text.input", org.testeditor.dsl.common.testing.DummyEnum.valueOf("enum_a"));
			reporter.leave(TestRunReporter.SemanticUnit.STEP, "Set enum of <Input> to \"enum_a\"", IDvar18, TestRunReporter.Status.OK, variables());
			reporter.leave(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication", IDvar1, TestRunReporter.Status.OK, variables());
			String IDvar19=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.MACRO_LIB, "GreetingApplicationMacro", IDvar19, TestRunReporter.Status.STARTED, variables());
			String IDvar20=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "TypeBoolean @jsonvar.\"key\" into input field", IDvar20, TestRunReporter.Status.STARTED, variables());
			org.junit.Assert.assertTrue("Parameter is expected to be of type = 'boolean' but a non coercible value = '"+jsonvar.getAsJsonObject().get("key").toString()+"' was passed through variable reference = 'jsonvar'.", jsonvar.getAsJsonObject().get("key").getAsJsonPrimitive().isBoolean());
			macro_GreetingApplicationMacro_TypeBoolIntoInputField(jsonvar.getAsJsonObject().get("key").getAsJsonPrimitive().getAsBoolean());
			reporter.leave(TestRunReporter.SemanticUnit.STEP, "TypeBoolean @jsonvar.\"key\" into input field", IDvar20, TestRunReporter.Status.OK, variables());
			String IDvar21=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "TypeLong @jsonvar.\"key\" into input field", IDvar21, TestRunReporter.Status.STARTED, variables());
			org.junit.Assert.assertTrue("Parameter is expected to be of type = 'long' but a non coercible value = '"+jsonvar.getAsJsonObject().get("key").toString()+"' was passed through variable reference = 'jsonvar'.", jsonvar.getAsJsonObject().get("key").getAsJsonPrimitive().isNumber());
			macro_GreetingApplicationMacro_TypeLongIntoInputField(jsonvar.getAsJsonObject().get("key").getAsJsonPrimitive().getAsLong());
			reporter.leave(TestRunReporter.SemanticUnit.STEP, "TypeLong @jsonvar.\"key\" into input field", IDvar21, TestRunReporter.Status.OK, variables());
			String IDvar22=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "TypeBoolean @stringvar into input field", IDvar22, TestRunReporter.Status.STARTED, variables());
			org.junit.Assert.assertTrue("Parameter is expected to be of type = 'boolean' but a non coercible value = '"+stringvar.toString()+"' was passed through variable reference = 'stringvar'.", Boolean.TRUE.toString().equals(stringvar) || Boolean.FALSE.toString().equals(stringvar));
			macro_GreetingApplicationMacro_TypeBoolIntoInputField(Boolean.valueOf(stringvar));
			reporter.leave(TestRunReporter.SemanticUnit.STEP, "TypeBoolean @stringvar into input field", IDvar22, TestRunReporter.Status.OK, variables());
			String IDvar23=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "TypeLong @stringvar into input field", IDvar23, TestRunReporter.Status.STARTED, variables());
			try { Long.parseLong(stringvar); } catch (NumberFormatException nfe) { org.junit.Assert.fail("Parameter is expected to be of type = 'long' but a non coercible value = '"+stringvar.toString()+"' was passed through variable reference = 'stringvar'."); }
			macro_GreetingApplicationMacro_TypeLongIntoInputField(Long.parseLong(stringvar));
		'''.indent(3))
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
			  try {
			    String IDvar0=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "Start the famous greetings application", IDvar0, TestRunReporter.Status.STARTED, variables());
			    String IDvar1=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication", IDvar1, TestRunReporter.Status.STARTED, variables());
			    String IDvar2=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Start application \"org.testeditor.swing.exammple.Greetings\"", IDvar2, TestRunReporter.Status.STARTED, variables());
			    dummyFixture.startApplication("org.testeditor.swing.exammple.Greetings");
			    reporter.leave(TestRunReporter.SemanticUnit.STEP, "Start application \"org.testeditor.swing.exammple.Greetings\"", IDvar2, TestRunReporter.Status.OK, variables());
			    String IDvar3=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "foo = Read list from <bar> [java.util.List<? extends java.lang.Object>]", IDvar3, TestRunReporter.Status.STARTED, variables());
			    java.util.List<? extends java.lang.Object> foo = dummyFixture.getList("label.greet");
			    reporter.leave(TestRunReporter.SemanticUnit.STEP, "foo = Read list from <bar> [java.util.List<? extends java.lang.Object>]", IDvar3, TestRunReporter.Status.OK, variables());
			    String IDvar4=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Stop application", IDvar4, TestRunReporter.Status.STARTED, variables());
			    dummyFixture.stopApplication();
			    reporter.leave(TestRunReporter.SemanticUnit.STEP, "Stop application", IDvar4, TestRunReporter.Status.OK, variables());
			    reporter.leave(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication", IDvar1, TestRunReporter.Status.OK, variables());
			    reporter.leave(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "Start the famous greetings application", IDvar0, TestRunReporter.Status.OK, variables());
			    String IDvar5=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "Do something different", IDvar5, TestRunReporter.Status.STARTED, variables());
			    reporter.leave(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "Do something different", IDvar5, TestRunReporter.Status.OK, variables());
			    finishedTestWith(TestRunReporter.Status.OK); // reaching this line of code means successful test execution
			  } catch (AssertionError e) {
			    reporter.assertionExit(e);
			    finishedTestWith(TestRunReporter.Status.ERROR);
			    org.junit.Assert.fail(e.getMessage());
			  } catch (org.testeditor.fixture.core.FixtureException e) {
			    reporter.fixtureExit(e);
			    finishedTestWith(TestRunReporter.Status.ABORTED);
			    org.junit.Assert.fail(e.getMessage());
			  } catch (Exception e) {
			    reporter.exceptionExit(e);
			    finishedTestWith(TestRunReporter.Status.ABORTED);
			    org.junit.Assert.fail(e.getMessage());
			  }
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
			String IDvar2=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Is <bar> visible ?", IDvar2, TestRunReporter.Status.STARTED, variables());
			dummyFixture.isVisible("label.greet");
		'''.indent(3))
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
			String IDvar2=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Set value of <Input> to \"theValue\"", IDvar2, TestRunReporter.Status.STARTED, variables());
			dummyFixture.setValue("text.input", "theValue");
		'''.indent(3))
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
			String IDvar2=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Set value \"theValue\" to <Input>", IDvar2, TestRunReporter.Status.STARTED, variables());
			dummyFixture.setValue("text.input", "theValue");
		'''.indent(3))
	}	

}
