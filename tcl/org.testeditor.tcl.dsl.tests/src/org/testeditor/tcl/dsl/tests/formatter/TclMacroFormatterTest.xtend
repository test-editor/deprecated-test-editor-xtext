package org.testeditor.tcl.dsl.tests.formatter

import org.junit.Ignore
import org.junit.Test

@Ignore("formatting is broken because of newline elements in the grammar. formatter is currently not in use (by the web frontend)!")
class TclMacroFormatterTest extends AbstractTclFormatterTest {

	val prefix = '''
		package com.example
		
		import a.b.c
		import c.d.e
		
		# MacroCollection
		
	'''

	@Test
	def void formatLineBreaks() {
		assertFormatted [
			expectation = prefix + '''
				## SomeMacro
				template = "do this" ${element} "and that" ${var}
				
					Component: some
				
				## OtherMacro
				template = "ok" ${element}
				
					Macro: MacroCollection
					- do this @element and that "some"
			'''

			toBeFormatted = prefix + '''
				## SomeMacro
				template =
				"do this"
				${element}
				"and that"
				${var}
				Component: some
				- some fixture
				## OtherMacro
				template =
				"ok"
				${element}
				Macro: MacroCollection
				- do
				this
				@element
				and
				that
				"some"
			'''
		]
	}

	@Test
	def void formatWhitespaces() {
		assertFormatted [
			expectation = prefix + '''
				## SomeMacro
				template = "do this" ${element} "and that" ${var}
				
					Component: some
				
				## OtherMacro
				template = "ok" ${element}
				
					Macro: MacroCollection
					- do this @element and "some" that @element
			'''

			toBeFormatted = prefix + '''
					  ##    SomeMacro   template 	= 	  "do this" 
				 ${element} 	  "and that" 	  ${var}	  	
				 Component      :     some    
				  	 ##OtherMacro		template = "ok" ${element}    
				  	 Macro    :   MacroCollection  	
					- do
				this    @    element     and    "some"    that    @element
			'''
		]
	}
}
