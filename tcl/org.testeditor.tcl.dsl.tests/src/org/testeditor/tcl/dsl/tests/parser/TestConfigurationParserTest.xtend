package org.testeditor.tcl.dsl.tests.parser

import com.google.inject.Inject
import com.google.inject.Provider
import org.eclipse.xtext.resource.XtextResourceSet
import org.junit.Test

class TestConfigurationParserTest extends AbstractParserTest {

	@Inject
	Provider<XtextResourceSet> resourceSetProvider

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

	@Test
	def void parseReferenceToConfig() {
		// given
		val resourceSet = resourceSetProvider.get
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
		val configModel = parseHelper.parse(configSource, resourceSet)
		val testModel = parseHelper.parse(testSource, resourceSet)

		// then
		configModel.assertNoSyntaxErrors
		testModel.assertNoSyntaxErrors
		testModel.test.config.assertSame(configModel.config)
	}

}
