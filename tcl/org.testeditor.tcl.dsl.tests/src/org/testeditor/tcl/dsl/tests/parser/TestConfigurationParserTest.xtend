package org.testeditor.tcl.dsl.tests.parser

import javax.inject.Inject
import org.junit.Test
import org.testeditor.dsl.common.testing.DslParseHelper
import org.testeditor.tcl.dsl.tests.AbstractTclTest

class TestConfigurationParserTest extends AbstractTclTest {

	@Inject extension DslParseHelper
	
	@Test
	def void parseMinimal() {
		// given
		val input = '''
			package com.example
			
			config MyConfig
		'''

		// when
		val model = parseTcl(input)

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
		val model = parseTcl(input)

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
		val model = parseTcl(input)

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
		val model = parseTcl(input)

		// then
		model.assertNoSyntaxErrors
		model.config => [
			setup.contexts.assertSingleElement
			cleanup.contexts.assertSingleElement
		]
	}

	@Test
	def void parseReferenceToConfig() {
		// given
		val configSource = '''
			package com.example
			
			config MyConfig
		'''
		val testSource = '''
			package com.example
			
			# MyTest
			
			config MyConfig
		'''

		// when
		val configModel = parseTcl(configSource)
		val testModel = parseTcl(testSource)

		// then
		configModel.assertNoSyntaxErrors
		testModel.assertNoSyntaxErrors
		testModel.test.config.assertSame(configModel.config)
	}

}
