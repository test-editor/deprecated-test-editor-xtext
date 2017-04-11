/*******************************************************************************
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 *******************************************************************************/
package org.testeditor.tcl.dsl.validation

import javax.inject.Inject
import org.eclipse.xtext.testing.validation.ValidationTestHelper
import org.junit.Test
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest

class TclValidatorTest extends AbstractParserTest {

	@Inject
	ValidationTestHelper validator

	@Test
	def void validateStringArray() {
		// given
		val aml = getAMLWithValueSpace('''#[ "New", "Open" ]''')
		var tcl = getTCLWithValue("Test", "New")

		parseAml(aml)
		var tclError = getTCLWithValue("Test2", "Save")

		// when
		var model = parseTcl(tcl.toString, "Test.tcl")
		var modelError = parseTcl(tclError.toString, "Test2.tcl")

		// then
		validator.assertNoIssues(model)
		assertFalse(validator.validate(modelError).isEmpty)
	}

	@Test
	def void validateNumberRange() {
		// given
		val aml = getAMLWithValueSpace("2 ... 5")
		var tcl = getTCLWithValue("Test", "4")

		parseAml(aml)
		var tclError = getTCLWithValue("Test2", "1")

		// when
		var model = parseTcl(tcl.toString, "Test.tcl")
		var modelError = parseTcl(tclError.toString, "Test2.tcl")

		// then
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

		// when
		var model = parseTcl(tcl.toString, "Test.tcl")
		var modelError = parseTcl(tclError.toString, "Test2.tcl")

		// then
		validator.assertNoIssues(model)
		assertFalse(validator.validate(modelError).isEmpty)
	}

	@Test
	def void testValidateFieldsWithManyValueSpaces() {
		// given
		val aml = getAMLWithValueSpace('''#["foo", "bar"]''')
		var tcl = getTCLWithTwoValueSpaces("Test", "foo", "Mask")
		parseAml(aml)

		var tclError = getTCLWithTwoValueSpaces("Test2", "fooHello", "Mask")

		// when
		var model = parseTcl(tcl.toString, "Test.tcl")
		var modelError = parseTcl(tclError.toString, "Test2.tcl")

		// then
		validator.assertNoIssues(model)
		assertFalse(validator.validate(modelError).isEmpty)
	}

	def getTCLWithTwoValueSpaces(String testName, String value1, String value2) {
		getTCLWithValue(testName, value1) + '''
			- execute menu item  "«value2»"  in tree <TestStepSelector>
		'''
	}

	def CharSequence getTCLWithValue(String testName, String value) {
		'''
			package com.example
			
			# «testName»
			* Start the famous greetings application
			Component: ProjectExplorer
			- execute menu item  "«value»"  in tree <ProjektBaum>
		'''
	}

	def CharSequence getAMLWithValueSpace(String valuespace) {
		'''
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
			value-space components = #["Mask", "Component"] 
			
			component type General {
			}
			
			
			component ProjectExplorer is General {
				element ProjektBaum is TreeView {
					label = "Projekt Baum"
					locator ="Project Explorer"
					executeContextMenuEntry.item restrict to projectmenues 
				}
				element TestStepSelector is TreeView {
					label = "Teststep selector"
					locator ="teststepSelector"
					executeContextMenuEntry.item restrict to components 
				}
			}
		'''
	}

}
