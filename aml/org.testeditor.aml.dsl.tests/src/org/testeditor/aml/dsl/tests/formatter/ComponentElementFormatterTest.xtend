package org.testeditor.aml.dsl.tests.formatter

import org.junit.Test

class ComponentElementFormatterTest extends AbstractFormatterTest {

	@Test
	def void valueSpaceRestrictionInNewLine() {
		// short syntax
		assertFormatted[
			toBeFormatted = '''
				element User is Text locate by DummyLocatorStrategy.ID "myId" doSomething.something restrict to items
			'''.wrapInComponent
			expectation = '''
				element User is Text locate by DummyLocatorStrategy.ID "myId"
					doSomething.something restrict to items
			'''.wrapInComponent
		]
		// long syntax
		assertFormatted[
			toBeFormatted = '''
				element User is Text { doSomething.something restrict to items }
			'''.wrapInComponent
			expectation = '''
				element User is Text { 
					doSomething.something restrict to items
				}
			'''.wrapInComponent
		]
	}

	private def String wrapInComponent(CharSequence input) '''
		component LoginPage is WebPage {
			
			«input»
			
		}
	'''

}
