package org.testeditor.tcl.dsl.tests.formatter

import org.junit.Test

class TestConfigurationFormatterTest extends AbstractTclFormatterTest {

	@Test
	def void formatMinimal() {
		assertFormatted[
			expectation = '''
				package com.example
				
				config MyConfig
			'''.trim
			toBeFormatted = expectation.toSingleLine
		]

	}

	@Test
	def void formatWithSetupAndCleanup() {
		assertFormatted[
			expectation = '''
				package com.example
				
				config MyConfig
				
				Setup:
				
					Component: Demo
					- some useful initialization
				
				Cleanup:
				
					Component: Demo
					- some useful teardown
			'''.trim
			toBeFormatted = expectation.toSingleLine
		]
	}

}
