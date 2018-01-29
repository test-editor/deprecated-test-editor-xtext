package org.testeditor.tcl.dsl.ide

import org.junit.Before
import org.junit.Test
import org.testeditor.dsl.common.testing.DummyFixture

class TclContentProposalProviderTest extends AbstractContentAssistTest {

	val star = #['*']
	val dashes = #[
		'-',
		'- '
	]
	val stepValueOrPunctuation = #[
		'"value"',
		'@',
		'?',
		'.'
	]
	val elementPunctuation = #[
		'<',
		'<>'
	]
	val expressionPrefix = #[
		'=',
		'['
	]
	val commandOrVariable = #[
		'assert',
		'name'
	]
	val value = #['value']
	val keywords = #[
		'Component',
		'Macro',
		'Mask'
	]
	val testSurroundKeywords = #[
		'Cleanup',
		'Setup'
	]
	val allTemplates = #[
		'Click on <element>',
		'Is <element> visible?',
		'Read bool from <element>',
		'Read confidential information from <element>',
		'Read enum from <element>',
		'Read jsonObject from <element>',
		'Read list from <element>',
		'Read long from <element>',
		'Read value from <element>',
		'Set enum of <element> to enum_a',
		'Set value of <element> to "value"',
		'Set value "value" to <element>',
		'Start application "path"',
		'Stop application',
		'TypeLong "1" into <element>',
		'Type boolean "true" into <element>',
		'Type confidential "param" into <element>',
		'Type "value" into <element>',
		'Type "value" into <element> and wait "1"',
		'Wait for "1" seconds'
	]

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
		proposals.expectOnly(allTemplates.prefixWithDash + dashes + keywords + testSurroundKeywords + star)
		proposals.expectNoneOf(allTemplates) // without dashes !
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
		proposals.expectOnly(allTemplates.prefixWithDash + dashes + keywords + stepValueOrPunctuation + elementPunctuation + value + star + testSurroundKeywords)
		proposals.expectNoneOf(allTemplates) // without dashes !
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
		proposals.expectOnly(allTemplates + dashes + keywords + stepValueOrPunctuation + commandOrVariable + elementPunctuation + value + star + testSurroundKeywords)
		proposals.expectNoneOf(allTemplates.prefixWithDash) // proposals must be without dashes! 
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
		val expectedTemplates = #['Start application "path"', 'Stop application']

		// when
		val proposals = tclSnippet.proposals

		// then
		proposals.expectOnly(expectedTemplates + dashes + expressionPrefix + stepValueOrPunctuation + elementPunctuation + star)
		proposals.expectNoneOf(allTemplates.without(expectedTemplates))
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
		proposals.expectOnly(#['application "path"'] + dashes + expressionPrefix + stepValueOrPunctuation + value + keywords + elementPunctuation + star +
			testSurroundKeywords)
		proposals.expect('application "path"').prefix.assertEquals('') 
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
		proposals.expectOnly(#[
			'boolean "true" into <element>',
			'confidential "param" into <element>',
			'"value" into <element>',
			'"value" into <element> and wait "1"'
		] + dashes + expressionPrefix + stepValueOrPunctuation + value + keywords + elementPunctuation + star + testSurroundKeywords)
		proposals.expect('"value" into <element>').prefix.assertEquals('')
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
		proposals.expectOnly(#['application "path"'] + dashes + stepValueOrPunctuation + elementPunctuation + star)
		proposals.expect('application "path"').prefix.assertEquals('app')
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
		proposals.expectOnly(#['application "path"'] + dashes + stepValueOrPunctuation + elementPunctuation + star)
		proposals.expect('application "path"').prefix.assertEquals('app')
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
		proposals.expectOnly(#['Input', 'bar'] + value + elementPunctuation)
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
		proposals.expectOnly(#['Input>', 'bar>'] + value + elementPunctuation)
		proposals.expect('Input>').prefix.assertEquals('<')
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
		proposals.expectOnly(#['bar', '>'])
		proposals.expect('bar').prefix.assertEquals('b')
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
		proposals.expectOnly(dashes + allTemplates.prefixWithDash + keywords + stepValueOrPunctuation + elementPunctuation + value + star + testSurroundKeywords)
		proposals.expectNoneOf(#[ 'Input', 'bar' ]) // 
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
		proposals.expectOnly(#[
			'TypeBoolean "true" into input field',
			'TypeLong "1" into input field',
			'TypeConfidential "param" into input field'
		] + dashes + keywords + stepValueOrPunctuation + value + elementPunctuation + star + testSurroundKeywords)
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
		proposals.expectOnly(#[
			'@boolParameter',
			'##'
		] + value + dashes + allTemplates + keywords + stepValueOrPunctuation + commandOrVariable + elementPunctuation)
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
		proposals.expectOnly(#[
			'@envVar',
			'@secretEnvVar',
			'envVar',
			'com.example.envVar',
			'secretEnvVar',
			'com.example.secretEnvVar'
		] + value + dashes + allTemplates + keywords + stepValueOrPunctuation + commandOrVariable + elementPunctuation + star + testSurroundKeywords)
		proposals => []
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

	private def Iterable<String> prefixWithDash(Iterable<String> originalStrings) {
		return originalStrings.map['''- «it»''']
	}
	
	private def Iterable<String> without(Iterable<String> originalStrings, Iterable<String> unwantedStrings) {
		originalStrings.filter[original|!unwantedStrings.exists[original == it]]
	}

}
