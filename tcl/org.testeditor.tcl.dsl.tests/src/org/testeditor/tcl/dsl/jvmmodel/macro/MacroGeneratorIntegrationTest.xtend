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
			
			## ReadMacro
				template = "Read some values"
				Component: GreetingApplication
				- value = Read value from <bar>
			
			## WriteMacro
				template = "Set input to" ${value}
				Component: GreetingApplication
				- Set value of <Input> to @value
			
			## WaitMacro
				template = "Wait for" ${x} "seconds"
				Component: GreetingApplication
				- Wait for @x seconds
			
			## SetValueAndWait
				template = "Read and write value and wait" ${seconds} "seconds"
				Component: GreetingApplication
				- value = Read value from <bar>
				Macro: MyMacroCollection
				- Set input to @value
				Macro: MyMacroCollection
				- Wait for @seconds seconds
		''')
	}

	@Test
	def void minimalGeneration() {
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
			- Wait for "5" seconds
		'''

		// when
		val generatedCode = tcl.parseAndGenerate

		// then
		generatedCode => [
			assertContains('''
				// Macro: MyMacroCollection
				// - Wait for "5" seconds
				macro_MyMacroCollection_WaitMacro(5);
			'''.indent(2))
			assertContains('''
				private void macro_MyMacroCollection_WaitMacro(final long x) throws Exception {
				  
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
				  // - Wait for @seconds seconds
				  macro_MyMacroCollection_WaitMacro(seconds);
				}
			'''.indent(1))

		]
	}

	// TODO unused variable
	// TODO environment variable

	override protected getTestHeader() '''
		«super.testHeader»
		
		* step1
	'''

}
