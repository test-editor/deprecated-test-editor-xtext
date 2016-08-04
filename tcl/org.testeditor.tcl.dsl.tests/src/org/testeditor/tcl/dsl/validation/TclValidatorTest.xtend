package org.testeditor.tcl.dsl.validation

import javax.inject.Inject
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.junit.Test
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest

class TclValidatorTest extends AbstractParserTest {

	@Inject
	ValidationTestHelper validator

	@Test
	def void validateStringArray() {
		//given
		val aml = getAMLWithValueSpace('''#[ "New", "Open" ]''')
		var tcl = getTCLWithValue("Test", "New")

		parseAml(aml)
		var tclError = getTCLWithValue("Test2", "Save")
		
		//when
		var model = parseTcl(tcl.toString, "Test.tcl")
		var modelError = parseTcl(tclError.toString, "Test2.tcl")
		
		//then
		validator.assertNoIssues(model)
		assertFalse(validator.validate(modelError).isEmpty)
	}

	@Test
	def void validateNumberRange() {
		//given
		val aml = getAMLWithValueSpace("2 ... 5")
		var tcl = getTCLWithValue("Test", "4")

		parseAml(aml)
		var tclError = getTCLWithValue("Test2", "1")
		
		//when
		var model = parseTcl(tcl.toString, "Test.tcl")
		var modelError = parseTcl(tclError.toString, "Test2.tcl")

		//then
		validator.assertNoIssues(model)
		assertFalse(validator.validate(modelError).isEmpty)
	}

	@Test
	def void validateRegEx() {
		// given
		val aml = getAMLWithValueSpace('''"^[a-zA-Z_0-9]"''')
		var tcl = getTCLWithValue("Test", "h")

		parseAml(aml)
		var tclError = getTCLWithValue("Test2", "!!hello")

		//when
		var model = parseTcl(tcl.toString, "Test.tcl")
		var modelError = parseTcl(tclError.toString, "Test2.tcl")

		//then
		validator.assertNoIssues(model)
		assertFalse(validator.validate(modelError).isEmpty)
	}

	def CharSequence getTCLWithValue(String testName, String value) { '''
			package com.example
			
			# «testName»
			* Start the famous greetings application
			Component: ProjectExplorer
			- execute menu item  "«value»"  in tree <ProjektBaum>
		'''
	}

	def CharSequence getAMLWithValueSpace(String valuespace) {'''
			package com.example
			
			interaction type executeContextMenuEntry {
				label = " execute context menu entry"
				template = "execute menu item " ${item} " in tree" ${element} 
				method = SWTFixture.executeContextMenuEntry(element,item)
			}
			
			element type TreeView {
				interactions =  executeContextMenuEntry
			}
			
			value-space projectmenues = «valuespace» 
			
			component type General {
			}
			
			
			component ProjectExplorer is General {
				element ProjektBaum is TreeView {
					label = "Projekt Baum"
					locator ="Project Explorer"
					executeContextMenuEntry.item restrict to projectmenues 
				}
			}
		'''
	}

}
