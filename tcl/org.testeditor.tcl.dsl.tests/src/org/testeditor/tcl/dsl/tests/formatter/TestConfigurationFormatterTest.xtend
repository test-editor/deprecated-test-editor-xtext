package org.testeditor.tcl.dsl.tests.formatter

import org.junit.Ignore
import org.junit.Test

@Ignore("formatting is broken because of newline elements in the grammar. formatter is currently not in use (by the web frontend)!")
class TestConfigurationFormatterTest extends AbstractTclFormatterTest {

	@Test
	def void formatMinimal() {
		assertFormatted[
			expectation = '''
				package com.example
				
				config MyConfig
			'''.trim
			toBeFormatted = '''
				package  com.example  config  MyConfig
			'''
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
			toBeFormatted = '''
				package  com.example  config  MyConfig  
				Setup:				
					Component:  Demo
					- some useful initialization
				Cleanup:
					Component: Demo
					- some useful teardown
			'''
		]
	}

}
