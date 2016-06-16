package org.testeditor.tml.dsl.tests.formatter

import org.junit.Test

class TmlModelFormatterTest extends AbstractTmlFormatterTest {

	@Test
	def void formatLineBreaks() {
		formatterTester.assertFormatted [
			expectation = '''
				package com.example
				
				import a.b.c
				import c.d.e
				
				# MacroCollection
				
				## SomeMacro
				template = "do this" ${element} "and that" ${var}
				Component: some
				- step withspaces "string" with <ele> and @some.
				- next step
				
				## OtherMacro
				template = "ok" ${element}
				Macro: MacroCollection
				- dos this @element and "some" that @element
			'''

			toBeFormatted = '''
				package com.example	import a.b.c
				import c.d.e
				#
				MacroCollection
				##
				SomeMacro
				template
				=
				"do this"
				${element}
				"and that"
				${var}
				Component
				:
				some
				-
				step
				withspaces
				"string"
				with
				<ele>
				and
				@
				some
				.
				-
				next
				step
				##
				OtherMacro
				template
				=
				"ok"
				${element}
				Macro
				:
				MacroCollection
				-
				dos
				this
				@
				element
				and
				"some"
				that
				@
				element
			'''
		]
	}

	@Test
	def void formatWhitespaces() {
		formatterTester.assertFormatted [
			expectation = '''
				package com.example
				
				import a.b.c
				import c.d.e
				
				# MacroCollection
				
				## SomeMacro
				template = "do this" ${element} "and that" ${var}
				Component: some
				- step withspaces "string" with <ele> and @some.
				- next step
				
				## OtherMacro
				template = "ok" ${element}
				Macro: MacroCollection
				- dos this @element and "some" that @element
			'''

			toBeFormatted = '''
				package com.example	import a.b.c
				import c.d.e 
				    #    MacroCollection   ##    SomeMacro   template 	= 	  "do this" 
				    ${element} 	  "and that" 	  ${var}	  	Component      :     some    -
				     step     withspaces        "string"       with
				      <ele>    and    @  some          .			-next     
				         step 	 ##OtherMacro		template = "ok" ${element}    Macro   
				         :   MacroCollection  	- dos
				   this    @    element     and    "some"    that    @element
			'''
		]
	}
}
