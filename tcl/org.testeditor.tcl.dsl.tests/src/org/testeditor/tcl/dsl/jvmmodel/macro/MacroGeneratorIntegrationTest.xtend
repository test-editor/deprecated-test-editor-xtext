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
		generatedCode => [
			assertContains('''
				@Test
				public void execute() throws Exception {
				  
				  reporter.enter(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "step1");
				  
				  // Macro: MyMacroCollection
				  // - Do nothing
				  macro_MyMacroCollection_EmptyMacro();
				}
			'''.indent(1))

			assertContains('''
				private void macro_MyMacroCollection_EmptyMacro() throws Exception {
				  
				}
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
		generatedCode => [
			assertContains('''
				// Macro: MyMacroCollection
				// - Do nothing with "x"
				macro_MyMacroCollection_EmptyMacroWithUnusedParameter("x");
			'''.indent(2))

			assertContains('''
				private void macro_MyMacroCollection_EmptyMacroWithUnusedParameter(final String unused) throws Exception {
				  
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
		generatedCode => [
			assertContains('''
				// Macro: MyMacroCollection
				// - Read some values
				macro_MyMacroCollection_ReadMacro();
				// - Read some values
				macro_MyMacroCollection_ReadMacro();
			'''.indent(2))
			assertContains('''
				private void macro_MyMacroCollection_ReadMacro() throws Exception {
				  
				  reporter.enter(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication");
				  
				  reporter.enter(TestRunReporter.SemanticUnit.STEP, "java.lang.String value = Read value from <bar>");
				  java.lang.String value = dummyFixture.getValue("label.greet");
				}
			'''.indent(1))
		]
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
		generatedCode => [
			assertContains('''
				// Macro: MyMacroCollection
				// - Sleep for "5" seconds
				macro_MyMacroCollection_SleepMacro(5);
			'''.indent(2))
			assertContains('''
				private void macro_MyMacroCollection_SleepMacro(final long x) throws Exception {
				  
				  reporter.enter(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication");
				  
				  reporter.enter(TestRunReporter.SemanticUnit.STEP, "Wait for @x seconds");
				  dummyFixture.waitSeconds(x);
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
		generatedCode => [
			assertContains('''
				// Macro: MyMacroCollection
				// - Do nothing nested
				macro_MyMacroCollection_EmptyNestedMacro();
			'''.indent(2))
			assertContains('''
				private void macro_MyMacroCollection_EmptyNestedMacro() throws Exception {
				  
				  // Macro: MyMacroCollection
				  // - Do nothing
				  macro_MyMacroCollection_EmptyMacro();
				}
			'''.indent(1))
			assertContains('''
				private void macro_MyMacroCollection_EmptyMacro() throws Exception {
				  
				}
			'''.indent(1))
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
		generatedCode => [
			assertContains('''
				// Macro: MyMacroCollection
				// - Read and write value and wait "5" seconds
				macro_MyMacroCollection_SetValueAndWait(5);
			'''.indent(2))
			assertContains('''
				private void macro_MyMacroCollection_SetValueAndWait(final long seconds) throws Exception {
				  
				  reporter.enter(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication");
				  
				  reporter.enter(TestRunReporter.SemanticUnit.STEP, "java.lang.String value = Read value from <bar>");
				  java.lang.String value = dummyFixture.getValue("label.greet");
				  // Macro: MyMacroCollection
				  // - Set input to @value
				  macro_MyMacroCollection_WriteMacro(value);
				  // Macro: MyMacroCollection
				  // - Sleep for @seconds seconds
				  macro_MyMacroCollection_SleepMacro(seconds);
				}
			'''.indent(1))

		]
	}

	@Test
	def void environmentVariableIsPassedToMacro() {
		// given
		val tcl = '''
			package com.example
			
			require myEnvVar
			
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
				// Macro: MyMacroCollection
				// - Sleep for @myEnvVar seconds
				try { Long.parseLong(env_myEnvVar); } catch (NumberFormatException nfe) { org.junit.Assert.fail("Parameter is expected to be of type = 'long' but a non coercible value = '"+env_myEnvVar.toString()+"' was passed through variable reference = 'myEnvVar'."); }
				macro_MyMacroCollection_SleepMacro(Long.parseLong(env_myEnvVar));
			'''.indent(2))
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
		generatedCode => [
			assertContains('''
				// Macro: MyMacroCollection
				// - stop this
				macro_MyMacroCollection_MacroWithNotExistingFixture();
			'''.indent(2))
			assertContains('''
				reporter.enter(TestRunReporter.SemanticUnit.COMPONENT, "GreetingApplication");
				
				reporter.enter(TestRunReporter.SemanticUnit.STEP, "Stop application");
				dummyFixture.stopApplication();
				org.junit.Assert.fail("Template 'do something' cannot be resolved with any known macro/fixture. Please check your Macro 'MyMacroCollection' in line 44.");
				'''.indent(2))
		]
	}

}
