package org.testeditor.tsl.dsl.parser

import org.junit.Test

class SimpleTslParserTest extends AbstractParserTest {

	@Test
	def void parseSpec() {
		// given
		val spec = '''
			This is the test spec Greetings.
			================================
			 
			In tis spec everything is a comment per default, expecting lines starting with *.
			 
			We can use some markdown elements to format the spec.
			Every comment is ignored. Only the lines starting with a *, are part of the formal specification.
			The specification is than used in the Test cases.
			So lets start with the formal specification for greetings:
			* Start the famous greetings application.
			* send greetings "Hello World" to the world.
			* shutdown the application
			 
			Thats all for this spec.
		'''
		
		// when
		val tsl = parse(spec)
		
		// then
		tsl.assertNoErrors
	}

}