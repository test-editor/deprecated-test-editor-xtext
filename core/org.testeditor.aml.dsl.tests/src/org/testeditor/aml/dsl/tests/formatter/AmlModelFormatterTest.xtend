package org.testeditor.aml.dsl.tests.formatter

import org.junit.Test

/**
 * General formatting tests for {@link AmlModel}, not specific
 * to a certain subtype of {@link ModelElement}.
 */
class AmlModelFormatterTest extends AbstractFormatterTest {

	@Test
	def void formatElementsAndBrackets() {
		formatterTester.assertFormatted[
			expectation = '''
				package com.example
				
				component type NoBrackets
				
				component type EmptyBrackets {
				}
				
				component type ContentWithin {
					label = "Hello"
				}
			'''

			toBeFormatted = '''
				package com.example	component type NoBrackets
					 component type EmptyBrackets{}
				component type ContentWithin{ label = "Hello"}
			'''
		]
	}
	
	@Test
	def void formatSpaces() {
		assertFormatted[
			expectation = '''
				component type Dialog
				
				interaction type MyInteractionType {
				}
				
				component MyDialog is Dialog {
					label = "myLabel"
				}
			'''
			toBeFormatted = '''
				component    type  	Dialog	
				 interaction 	type  MyInteractionType   { }
				component MyDialog    is	Dialog {  label  = 	"myLabel" }
			'''
		]		
	}

}