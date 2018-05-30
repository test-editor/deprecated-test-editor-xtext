/*******************************************************************************
 * Copyright (c) 2012 - 2018 Signal Iduna Corporation and others.
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
import org.eclipse.xtext.diagnostics.Severity
import org.eclipse.xtext.testing.validation.ValidationTestHelper
import org.eclipse.xtext.validation.Issue
import org.eclipse.xtext.xbase.lib.Functions.Function1
import org.junit.Test
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest

class TclValidatorTest extends AbstractParserTest {

	@Inject
	ValidationTestHelper validator

	@Test
	def void validateStringArray() {
		// given
		val Function1<Issue, Boolean> warningPredicate = [
			message.matches(".*Allowed values: '\\[New, Open\\]'.*") && severity == Severity.WARNING
		]
		getAMLWithValueSpace('''#[ "New", "Open" ]''').parseAml

		var tcl = getTCLWithValue("Test", "New")
		var tclExpectingWarning = getTCLWithValue("Test2", "Save")

		// when
		var model = parseTcl(tcl.toString, "Test.tcl")
		var modelExpectingWarning = parseTcl(tclExpectingWarning.toString, "Test2.tcl")

		// then
		validator.validate(model).assertNotExists(warningPredicate, model.reportableValidations)
		validator.validate(modelExpectingWarning).assertExists(warningPredicate, modelExpectingWarning.reportableValidations)
	}

	@Test
	def void validateNumberRange() {
		// given
		val Function1<Issue, Boolean> warningPredicate = [
			message.matches(".*Allowed values: '2 <= x <= 5'.*") && severity == Severity.WARNING
		]
		getAMLWithValueSpace("2 ... 5").parseAml

		var tcl = getTCLWithValue("Test", "4")
		var tclExpectingWarning = getTCLWithValue("Test2", "1")

		// when
		var model = parseTcl(tcl.toString, "Test.tcl")
		var modelExpectingWarning = parseTcl(tclExpectingWarning.toString, "Test2.tcl")

		// then
		validator.validate(model).assertNotExists(warningPredicate, model.reportableValidations)
		validator.validate(modelExpectingWarning).assertExists(warningPredicate, modelExpectingWarning.reportableValidations)
	}

	@Test
	def void validateRegEx() {
		// given
		val Function1<Issue, Boolean> warningPredicate = [
			message.matches(".*Allowed values: 'Regular expression: \\^\\[a-zA-Z_0-9\\]'.*") && severity == Severity.WARNING
		]
		getAMLWithValueSpace('''"^[a-zA-Z_0-9]"''').parseAml

		var tcl = getTCLWithValue("Test", "h")
		var tclExpectingWarning = getTCLWithValue("Test2", "!!hello")

		// when
		var model = parseTcl(tcl.toString, "Test.tcl")
		var modelExpectingWarning = parseTcl(tclExpectingWarning.toString, "Test2.tcl")

		// then
		validator.validate(model).assertNotExists(warningPredicate, model.reportableValidations)
		validator.validate(modelExpectingWarning).assertExists(warningPredicate, modelExpectingWarning.reportableValidations)
	}

	@Test
	def void testValidateFieldsWithManyValueSpaces() {
		// given
		val Function1<Issue, Boolean> warningPredicate = [
			message.matches(".*Allowed values: '\\[foo, bar\\]'.*") && severity == Severity.WARNING
		]
		getAMLWithValueSpace('''#["foo", "bar"]''').parseAml

		var tcl = getTCLWithTwoValueSpaces("Test", "foo", "Mask")
		var tclExpectingWarning = getTCLWithTwoValueSpaces("Test2", "fooHello", "Mask")

		// when
		var model = parseTcl(tcl.toString, "Test.tcl")
		var modelExpectingWarning = parseTcl(tclExpectingWarning.toString, "Test2.tcl")

		// then
		validator.validate(model).assertNotExists(warningPredicate, model.reportableValidations)
		validator.validate(modelExpectingWarning).assertExists(warningPredicate, modelExpectingWarning.reportableValidations)
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
				method = AnyFixture.executeContextMenuEntry(element,item)
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

	private def String reportableValidations(TclModel model) {
		return '''got: «validator.validate(model).map[toString].join('\n')»'''
	}

}
