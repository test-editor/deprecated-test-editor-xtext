package org.testeditor.tcl.dsl.jvmmodel.macro

import org.junit.Before
import org.junit.Test
import org.testeditor.dsl.common.testing.DummyFixture

class MacroGeneratorIntegrationTest extends org.testeditor.tcl.dsl.jvmmodel.AbstractTclGeneratorIntegrationTest {

	@Before
	def void setup() {
		parseAml(DummyFixture.amlModel)
		parseTcl('''
			package com.example
			
			# MyMacroCollection
			
			## EmptyMacro
				template = "Do nothing"
			
			## EmptyNestedMacro
				template = "Do nothing nested"
				Macro: MyMacroCollection
				- Do nothing
			
			## EmptyMacroWithUnusedParameter
				template = "Do nothing with" ${unused}
			
			## ReadMacro
				template = "Read some values"
				Component: GreetingApplication
				- value = Read value from <bar>
			
			## WriteMacro
				template = "Set input to" ${value}
				Component: GreetingApplication
				- Set value of <Input> to @value
			
			## SleepMacro
				template = "Sleep for" ${x} "seconds"
				Component: GreetingApplication
				- Wait for @x seconds
			
			## SetValueAndWait
				template = "Read and write value and wait" ${seconds} "seconds"
				Component: GreetingApplication
				- value = Read value from <bar>
				Macro: MyMacroCollection
				- Set input to @value
				Macro: MyMacroCollection
				- Sleep for @seconds seconds
			
			## MacroWithNotExistingFixture
				template = "stop this"
				Component: GreetingApplication
				- Stop application
				- do something
		''')
	}

	@Test
	def void emptyMacroWithoutParameter() {
		// given
		val tcl = '''
			Macro: MyMacroCollection
			- Do nothing
		'''

		// when
		val generatedCode = tcl.parseAndGenerate

		// then
		generatedCode.replaceIDVarNumbering => [
			assertContains('''
				@Test
				public void execute() throws Exception {
				  try {
				    String IDvar=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "step1", IDvar, TestRunReporter.Status.STARTED, variables());
				    String IDvar=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.MACRO_LIB, "MyMacroCollection", IDvar, TestRunReporter.Status.STARTED, variables());
				    String IDvar=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Do nothing", IDvar, TestRunReporter.Status.STARTED, variables());
				    macro_MyMacroCollection_EmptyMacro();
			'''.indent(1))

			assertContains('''
				private void macro_MyMacroCollection_EmptyMacro() throws Exception {
				  try {
				    String IDvar=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.MACRO, "EmptyMacro", IDvar, TestRunReporter.Status.STARTED, variables());
				    reporter.leave(TestRunReporter.SemanticUnit.MACRO, "EmptyMacro", IDvar, TestRunReporter.Status.OK, variables());
			'''.indent(1))
		]
	}

	@Test
	def void emptyMacroWithUnusedParameter() {
		// given
		val tcl = '''
			Macro: MyMacroCollection
			- Do nothing with "x"
		'''

		// when
		val generatedCode = tcl.parseAndGenerate

		// then
		generatedCode.replaceIDVarNumbering => [
			assertContains('''
				macro_MyMacroCollection_EmptyMacroWithUnusedParameter("x");
			'''.indent(2))

			assertContains('''
				private void macro_MyMacroCollection_EmptyMacroWithUnusedParameter(final String unused) throws Exception {
				  try {
				    String IDvar=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.MACRO, "EmptyMacroWithUnusedParameter", IDvar, TestRunReporter.Status.STARTED, variables());
				    reporter.leave(TestRunReporter.SemanticUnit.MACRO, "EmptyMacroWithUnusedParameter", IDvar, TestRunReporter.Status.OK, variables());
				  } catch (AssertionError e) {
				    reporter.reportAssertionExit(e);
				    finishedTestWith(TestRunReporter.Status.ERROR);
				    org.junit.Assert.fail(e.getMessage());
				  } catch (org.testeditor.fixture.core.FixtureException e) {
				    reporter.reportFixtureExit(e);
				    finishedTestWith(TestRunReporter.Status.ABORTED);
				    org.junit.Assert.fail(e.getMessage());
				  } catch (Exception e) {
				    reporter.reportExceptionExit(e);
				    finishedTestWith(TestRunReporter.Status.ABORTED);
				    org.junit.Assert.fail(e.getMessage());
				  }
				}
  			'''.indent(1))
		]
	}

	@Test
	def void repeatedMacroInvocation() {
		// given
		val tcl = '''
			Macro: MyMacroCollection
			- Read some values
			- Read some values
		'''

		// when
		val generatedCode = tcl.parseAndGenerate

		// then
		generatedCode.replaceIDVarNumbering => [
			assertContains('''
				macro_MyMacroCollection_ReadMacro();
				reporter.leave(TestRunReporter.SemanticUnit.STEP, "Read some values", IDvar, TestRunReporter.Status.OK, variables());
				String IDvar=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Read some values", IDvar, TestRunReporter.Status.STARTED, variables());
				macro_MyMacroCollection_ReadMacro();
			'''.indent(3))
			assertContains('''
			  private void macro_MyMacroCollection_ReadMacro() throws Exception {
			    try {
			      String IDvar=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.MACRO, "ReadMacro", IDvar, TestRunReporter.Status.STARTED, variables());
			      String IDvar=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication", IDvar, TestRunReporter.Status.STARTED, variables());
			      String IDvar=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "value = Read value from <bar> [java.lang.String]", IDvar, TestRunReporter.Status.STARTED, variables());
			      java.lang.String value = dummyFixture.getValue("label.greet");
			'''.indent(1))
		]
	}
	
	def private String replaceIDVarNumbering(String codeblock) {
		return codeblock.replaceAll('IDvar[0-9]*', 'IDvar')
	} 

	@Test
	def void macroWithParameter() {
		// given
		val tcl = '''
			Macro: MyMacroCollection
			- Sleep for "5" seconds
		'''

		// when
		val generatedCode = tcl.parseAndGenerate

		// then
		generatedCode.replaceIDVarNumbering => [
			assertContains('''
				macro_MyMacroCollection_SleepMacro(5);
			'''.indent(2))
			assertContains('''
				private void macro_MyMacroCollection_SleepMacro(final long x) throws Exception {
				  try {
				    String IDvar=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.MACRO, "SleepMacro", IDvar, TestRunReporter.Status.STARTED, variables());
				    String IDvar=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication", IDvar, TestRunReporter.Status.STARTED, variables());
				    String IDvar=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Wait for @x seconds", IDvar, TestRunReporter.Status.STARTED, variables("x", Long.toString(x)));
				    dummyFixture.waitSeconds(x);
				    reporter.leave(TestRunReporter.SemanticUnit.STEP, "Wait for @x seconds", IDvar, TestRunReporter.Status.OK, variables("x", Long.toString(x)));
				    reporter.leave(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication", IDvar, TestRunReporter.Status.OK, variables());
				    reporter.leave(TestRunReporter.SemanticUnit.MACRO, "SleepMacro", IDvar, TestRunReporter.Status.OK, variables());
				  } catch (AssertionError e) {
				    reporter.reportAssertionExit(e);
				    finishedTestWith(TestRunReporter.Status.ERROR);
				    org.junit.Assert.fail(e.getMessage());
				  } catch (org.testeditor.fixture.core.FixtureException e) {
				    reporter.reportFixtureExit(e);
				    finishedTestWith(TestRunReporter.Status.ABORTED);
				    org.junit.Assert.fail(e.getMessage());
				  } catch (Exception e) {
				    reporter.reportExceptionExit(e);
				    finishedTestWith(TestRunReporter.Status.ABORTED);
				    org.junit.Assert.fail(e.getMessage());
				  }
				}
    			'''.indent(1))
		]
	}

	@Test
	def void emptyNestedMacro() {
		// given
		val tcl = '''
			Macro: MyMacroCollection
			- Do nothing nested
		'''

		// when
		val generatedCode = tcl.parseAndGenerate

		// then
		generatedCode.replaceIDVarNumbering => [
			assertContains('''
				macro_MyMacroCollection_EmptyNestedMacro();
			'''.indent(2))
			assertContains('''
				macro_MyMacroCollection_EmptyMacro();
			'''.indent(1))
			assertContains('''
				String IDvar=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.MACRO, "EmptyMacro", IDvar, TestRunReporter.Status.STARTED, variables());
				reporter.leave(TestRunReporter.SemanticUnit.MACRO, "EmptyMacro", IDvar, TestRunReporter.Status.OK, variables());
 			'''.indent(3))
		]
	}

	@Test
	def void nestedMacroWithMultipleVariables() {
		// given
		val tcl = '''
			Macro: MyMacroCollection
			- Read and write value and wait "5" seconds
		'''

		// when
		val generatedCode = tcl.parseAndGenerate

		// then
		generatedCode.replaceIDVarNumbering => [
			assertContains('''
				macro_MyMacroCollection_SetValueAndWait(5);
			'''.indent(2))
			assertContains('''
				private void macro_MyMacroCollection_SetValueAndWait(final long seconds) throws Exception {
				  try {
				    String IDvar=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.MACRO, "SetValueAndWait", IDvar, TestRunReporter.Status.STARTED, variables());
				    String IDvar=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication", IDvar, TestRunReporter.Status.STARTED, variables());
				    String IDvar=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "value = Read value from <bar> [java.lang.String]", IDvar, TestRunReporter.Status.STARTED, variables());
				    java.lang.String value = dummyFixture.getValue("label.greet");
				    reporter.leave(TestRunReporter.SemanticUnit.STEP, "value = Read value from <bar> [java.lang.String]", IDvar, TestRunReporter.Status.OK, variables());
				    reporter.leave(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication", IDvar, TestRunReporter.Status.OK, variables());
				    String IDvar=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.MACRO_LIB, "MyMacroCollection", IDvar, TestRunReporter.Status.STARTED, variables());
				    String IDvar=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Set input to @value", IDvar, TestRunReporter.Status.STARTED, variables());
				    macro_MyMacroCollection_WriteMacro(value);
				    reporter.leave(TestRunReporter.SemanticUnit.STEP, "Set input to @value", IDvar, TestRunReporter.Status.OK, variables());
				    reporter.leave(TestRunReporter.SemanticUnit.MACRO_LIB, "MyMacroCollection", IDvar, TestRunReporter.Status.OK, variables());
				    String IDvar=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.MACRO_LIB, "MyMacroCollection", IDvar, TestRunReporter.Status.STARTED, variables());
				    String IDvar=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Sleep for @seconds seconds", IDvar, TestRunReporter.Status.STARTED, variables());
				    macro_MyMacroCollection_SleepMacro(seconds);
				    reporter.leave(TestRunReporter.SemanticUnit.STEP, "Sleep for @seconds seconds", IDvar, TestRunReporter.Status.OK, variables());
				    reporter.leave(TestRunReporter.SemanticUnit.MACRO_LIB, "MyMacroCollection", IDvar, TestRunReporter.Status.OK, variables());
				    reporter.leave(TestRunReporter.SemanticUnit.MACRO, "SetValueAndWait", IDvar, TestRunReporter.Status.OK, variables());
				  } catch (AssertionError e) {
				    reporter.reportAssertionExit(e);
				    finishedTestWith(TestRunReporter.Status.ERROR);
				    org.junit.Assert.fail(e.getMessage());
				  } catch (org.testeditor.fixture.core.FixtureException e) {
				    reporter.reportFixtureExit(e);
				    finishedTestWith(TestRunReporter.Status.ABORTED);
				    org.junit.Assert.fail(e.getMessage());
				  } catch (Exception e) {
				    reporter.reportExceptionExit(e);
				    finishedTestWith(TestRunReporter.Status.ABORTED);
				    org.junit.Assert.fail(e.getMessage());
				  }
				}
			'''.indent(1))

		]
	}

	@Test
	def void environmentVariableIsPassedToMacro() {
		// given
		val tcl = '''
			package com.example
			
			require public myEnvVar
			
			# SimpleTest
			* step1
				Macro: MyMacroCollection
				- Sleep for @myEnvVar seconds
		'''

		// when
		val tclModel = parseTcl(tcl, "SimpleTest.tcl")
		val generatedCode = tclModel.generate

		// then
		generatedCode => [
			assertContains('''
				try { Long.parseLong(env_myEnvVar); } catch (NumberFormatException nfe) { org.junit.Assert.fail("Parameter is expected to be of type = 'long' but a non coercible value = '"+env_myEnvVar.toString()+"' was passed through variable reference = 'myEnvVar'."); }
				macro_MyMacroCollection_SleepMacro(Long.parseLong(env_myEnvVar));
			'''.indent(3))
		]
	}

	override protected getTestHeader() '''
		«super.testHeader»
		
		* step1
	'''

	@Test
	def void testNotExistingFixtureInMacro() {
		// given
		val tcl = '''
			Macro: MyMacroCollection
			- stop this
		'''

		// when
		val generatedCode = tcl.parseAndGenerate


		// then
		generatedCode.replaceIDVarNumbering => [
			assertContains('''
				macro_MyMacroCollection_MacroWithNotExistingFixture();
			'''.indent(2))
			assertContains('''
				String IDvar=newVarId(); reporter.enter(TestRunReporter.SemanticUnit.STEP, "Stop application", IDvar, TestRunReporter.Status.STARTED, variables());
				dummyFixture.stopApplication();
				reporter.leave(TestRunReporter.SemanticUnit.STEP, "Stop application", IDvar, TestRunReporter.Status.OK, variables());
				org.junit.Assert.fail("Template 'do something' cannot be resolved with any known macro/fixture. Please check your Macro 'MyMacroCollection' in line 44.");
			'''.indent(3))
		]
	}

}
