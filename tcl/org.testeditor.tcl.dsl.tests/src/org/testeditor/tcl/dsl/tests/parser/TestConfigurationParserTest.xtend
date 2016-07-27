package org.testeditor.tcl.dsl.tests.parser

import org.junit.Test

class TestConfigurationParserTest extends AbstractParserTest {

	@Test
	def void parseMinimal() {
		// given
		val input = '''
			package com.example
			
			config MyConfig
		'''

		// when
		val model = parseHelper.parse(input)

		// then
		model.assertNoSyntaxErrors
		model.package.assertEquals('com.example')
		model.config => [
			name.assertEquals('MyConfig')
		]
	}

	@Test
	def void parseWithEmptySetupAndCleanup() {
		// given
		val input = '''
			package com.example
						
			config MyConfig
			
			Setup:
			Cleanup:
		'''

		// when
		val model = parseHelper.parse(input)

		// then
		model.assertNoSyntaxErrors
		model.config => [
			setup.assertNotNull
			cleanup.assertNotNull
		]
	}

	@Test
	def void parseWithEmptyCleanupAndSetup() {
		// given
		val input = '''
			package com.example
						
			config MyConfig
			
			Cleanup:
			Setup:
		'''

		// when
		val model = parseHelper.parse(input)

		// then
		model.assertNoSyntaxErrors
		model.config => [
			setup.assertNotNull
			cleanup.assertNotNull
		]
	}

	@Test
	def void parseWithSetupAndCleanup() {
		// given
		val input = '''
			package com.example
						
			config MyConfig
			
			Setup:
			
				Component: Demo
				- some useful initialization
			
			Cleanup:
			
				Component: Demo
				- some useful teardown
		'''

		// when
		val model = parseHelper.parse(input)

		// then
		model.assertNoSyntaxErrors
		model.config => [
			setup.contexts.assertSingleElement
			cleanup.contexts.assertSingleElement
		]
	}

}
