package org.testeditor.tcl.dsl.ide

import org.junit.Before
import org.junit.Test
import org.testeditor.dsl.common.testing.DummyFixture

class TclContentProposalProviderTest extends AbstractContentAssistTest {

	@Before
	def void parseAmlModel() {
		parseAml(DummyFixture.amlModel)
		parseTcl(DummyFixture.getMacroModel('MacroLibrary'))
	}

	@Test
	def void testComponentTemplateProposalWithoutDash() {
		// given
		var tclSnippet = '''
			# Some
			
			* some step
			Component: GreetingApplication
			|
		'''.withPackage

		// when
		val proposals = tclSnippet.proposals

		// then
		proposals => [
			expect('- Stop application')
			expect('- Start application "path"')
			expect('- Is <element> visible?')
			expect('- Type "value" into <element> and wait "1"')
			expect('- Wait for "1" seconds')
			reject('Stop application') // proposals must include '-'
			reject('com.example.start.path') // crossreference to template variable must not show
		]
	}
	
	@Test
	def void testComponentSecondTemplateProposalWithoutDash() {
		// given
		var tclSnippet = '''
			# Some
			
			* some step
			Component: GreetingApplication
			- Stop application
			|
		'''.withPackage

		// when
		val proposals = tclSnippet.proposals

		// then
		proposals => [
			expect('- Stop application')
			expect('- Start application "path"')
			expect('- Is <element> visible?')
			expect('- Type "value" into <element> and wait "1"')
			expect('- Wait for "1" seconds')
			reject('Stop application') // proposal must include '-'
			reject('com.example.start.path') // crossreference to template variable must not show
		]
	}
	
	@Test
	def void testComponentTemplateProposal() {
		// given
		var tclSnippet = '''
			# Some
			
			* some step
			Component: GreetingApplication
			- |
		'''.withPackage

		// when
		val proposals = tclSnippet.proposals

		// then
		proposals => [
			expect('Stop application')
			expect('Start application "path"')
			expect('Is <element> visible?')
			expect('Type "value" into <element> and wait "1"')
			expect('Wait for "1" seconds')
			reject('- Stop application') // proposal must NOT include '-'
			reject('com.example.start.path') // crossreference to template variable must not show
		]
	}
	
	@Test
	def void testComponentIncompleteTemplateProposal() {
		// given
		var tclSnippet = '''
			# Some
			
			* some step
			Component: GreetingApplication
			- St|
		'''.withPackage

		// when
		val proposals = tclSnippet.proposals

		// then
		proposals => [
			expect('Stop application').prefix.assertEquals('St')
			expect('Start application "path"').prefix.assertEquals('St')
			reject('Wait for "1" seconds')
			reject('- Stop application') // proposal must NOT include '-'
		]
	}

	@Test
	def void testComponentIncompleteSpacedTemplateProposal() {
		// given
		var tclSnippet = '''
			# Some
			
			* some step
			Component: GreetingApplication
			- Start |
		'''.withPackage

		// when
		val proposals = tclSnippet.proposals

		// then
		proposals => [
			expect('application "path"').prefix.assertEquals('')
			reject('Stop application') // proposal must NOT include templates with different prefix
		]
	}


	@Test
	def void testComponentIncompleteMultipartMultiTemplateProposal() {
		// given
		var tclSnippet = '''
			# Some
			
			* some step
			Component: GreetingApplication
			- Type |
		'''.withPackage

		// when
		val proposals = tclSnippet.proposals

		// then
		proposals => [
			expect('boolean "true" into <element>').prefix.assertEquals('')
			expect('"value" into <element> and wait "1"').prefix.assertEquals('')
			expect('"value" into <element>').prefix.assertEquals('')
			expect('confidential "param" into <element>').prefix.assertEquals('')
			reject('Stop application') // proposal must NOT include templates with different prefix
		]
	}

	@Test
	def void testComponentIncompleteMultipartTemplateProposal() {
		// given
		var tclSnippet = '''
			# Some
			
			* some step
			Component: GreetingApplication
			- Start app|
		'''.withPackage

		// when
		val proposals = tclSnippet.proposals

		// then
		proposals => [
			expect('application "path"').prefix.assertEquals('app')
			reject('Stop application') // proposal must NOT include templates with different prefix
		]
	}

	@Test
	def void testComponentDenormalizedIncompleteMultipartTemplateProposal() {
		// given
		var tclSnippet = '''
			# Some
			
			* some step
			Component: GreetingApplication
			      - 	Start   	 app|
		'''.withPackage
	
		// when
		val proposals = tclSnippet.proposals

		// then
		proposals => [
			expect('application "path"').prefix.assertEquals('app')
			reject('Stop application')
		]
	}

	@Test
	def void testElementProposal() {
		// given
		var tclSnippet = '''
			# Some
			
			* some step
			Component: GreetingApplication
			- Read value from <|>
		'''.withPackage

		// when
		val proposals = tclSnippet.proposals

		// then
		proposals => [
			expect('Input').prefix.assertEquals('<')
			expect('bar').prefix.assertEquals('<')
			reject('Ok') // buttons have no 'Read value from' - fixture
		]
	}

	@Test
	def void testElementProposalNoClosingBracket() {
		// given
		var tclSnippet = '''
			# Some
			
			* some step
			Component: GreetingApplication
			- Read value from <|
		'''.withPackage

		// when
		val proposals = tclSnippet.proposals

		// then
		proposals => [
			expect('Input>').prefix.assertEquals('<')
			expect('bar>').prefix.assertEquals('<')
			reject('Ok>') // buttons have no 'Read value from' - fixture
		]
	}

	@Test
	def void testIncompleteElementProposal() {
		// given
		var tclSnippet = '''
			# Some
			
			* some step
			Component: GreetingApplication
			- Read value from <b|>
		'''.withPackage

		// when
		val proposals = tclSnippet.proposals

		// then
		proposals => [
			reject('Input')
			expect('bar').prefix.assertEquals('b')
			reject('Ok') 
		]
	}

	@Test
	def void testTrailingElementProposal() {
		// given
		var tclSnippet = '''
			# Some
			
			* some step
			Component: GreetingApplication
			- Read value from <Input> |
		'''.withPackage

		// when
		val proposals = tclSnippet.proposals

		// then
		proposals => [
			reject('Input>')
			reject('bar>')
			reject('Ok>') 
		]
	}

	@Test
	def void testMacroTemplateProposal() {
		// given
		var tclSnippet = '''
			# Some
			
			* some step
			Macro: MacroLibrary
			- |
		'''.withPackage		

		// when
		val proposals = tclSnippet.proposals

		// then
		proposals => [
			expect('TypeBoolean "true" into input field')
			expect('TypeLong "1" into input field')
			expect('TypeConfidential "param" into input field')
		]
	}
	
	@Test
	def void testMacroParameterProposal() {
		// given
		var tclSnippet = '''
			# MyOwnMacro
			
			## TypeBoolIntoInputField
			template = "TypeBoolean" ${boolParameter} "into input field"
			Component: GreetingApplication
			- |
		'''.withPackage		

		// when
		val proposals = tclSnippet.proposals

		// then
		proposals => [
			expect('@boolParameter')
		]
	}
	
	@Test
	def void testEnvironmentParameter() {
		// given
		var tclSnippet = '''
			require public envVar, secretEnvVar
			
			# Some
			
			* some step
			Component: GreetingApplication
			- |
		'''.withPackage

		// when
		val proposals = tclSnippet.proposals

		// then
		proposals => [
			expect('@envVar')
			expect('@secretEnvVar')
		]
	}
	
	/**
	 * put 'package com.example' before the actual source code
	 */
	private def String withPackage(CharSequence source) {
		return '''
			package com.example
			
			«source»
		'''
	}
	
}
