package org.testeditor.tcl.util

import javax.inject.Inject
import org.eclipse.xtext.resource.XtextResourceSet
import org.junit.Before
import org.junit.Test
import org.testeditor.aml.dsl.tests.AmlModelGenerator
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest
import org.testeditor.tsl.StepContentVariable

/**
 * Kind of in-between a unit and integration test here. We parse some example
 * models instead of creating them programmatically to make them easier to understand.
 * 
 * The example data is the same for all tests, but the data model is modified within
 * the individual tests. This way we don't need to copy & paste the same text over and over again.
 */
class ValueSpaceHelperTest extends AbstractParserTest {

	@Inject extension ValueSpaceHelper
	@Inject extension AmlModelGenerator
	@Inject ExampleAmlModel amlModel

	// TCL elements
	StepContentVariable monthValue
	StepContentVariable yearValue
	StepContentVariable secondsValue
	StepContentVariable xValue

	@Before
	def void parseExampleData() {
		resourceSet = amlModel.model.eResource.resourceSet as XtextResourceSet
		// The test itself that uses the defined interaction
		val tcl = '''
			package com.example
			
			# MyTest
			
			* dummy
			
			Component: MyDialog
			- Enter month "10" and year "2000" into <Date>
			- Wait for "3" seconds
			- Unresolved interaction "x"
		'''
		parseTcl(tcl, "MyTest.tcl") => [
			assertNoErrors
			// extract instance variables for easier access
			val variables = eAllContents.filter(StepContentVariable).toList
			monthValue = variables.findFirst[value == "10"]
			yearValue = variables.findFirst[value == "2000"]
			secondsValue = variables.findFirst[value == "3"]
			xValue = variables.findFirst[value == "x"]
		]
	}

	@Test
	def void noValueSpaceRestrictions() {
		// when + then
		monthValue.valueSpace.assertAbsent
		yearValue.valueSpace.assertAbsent
		xValue.valueSpace.assertAbsent
	}

	@Test
	def void restrictionOnInteractionMonth() {
		// given
		amlModel.enterMonthAndYear.valueSpaceAssignments += amlModel.monthVariable.restrictTo(amlModel.validMonth)

		// when + then
		monthValue.valueSpace.assertPresent.assertSame(amlModel.validMonth)
		yearValue.valueSpace.assertAbsent
	}

	@Test
	def void restrictionOnInteractionYear() {
		// given
		amlModel.enterMonthAndYear.valueSpaceAssignments += amlModel.yearVariable.restrictTo(amlModel.validYear)

		// when + then
		monthValue.valueSpace.assertAbsent
		yearValue.valueSpace.assertPresent.assertSame(amlModel.validYear)
	}

	@Test
	def void restrictionOnComponentElement() {
		// given
		amlModel.date.valueSpaceAssignments += amlModel.monthVariable.restrictTo(amlModel.validMonth)

		// when + then
		monthValue.valueSpace.assertPresent.assertSame(amlModel.validMonth)
	}

	@Test
	def void restrictionOnComponentElementType() {
		// given
		amlModel.dateText.valueSpaceAssignments += amlModel.monthVariable.restrictTo(amlModel.validMonth)

		// when + then
		monthValue.valueSpace.assertPresent.assertSame(amlModel.validMonth)
	}

	@Test
	def void elementRestrictionOverwritesOtherRestrictions() {
		// given
		val january = integerRange("JanuaryOnly", 1, 1)
		amlModel.date.valueSpaceAssignments += amlModel.monthVariable.restrictTo(january)
		amlModel.dateText.valueSpaceAssignments += amlModel.monthVariable.restrictTo(amlModel.validMonth)
		amlModel.enterMonthAndYear.valueSpaceAssignments += amlModel.monthVariable.restrictTo(amlModel.validMonth)

		// when + then
		monthValue.valueSpace.assertPresent.assertSame(january)
	}

	@Test
	def void elementTypeRestrictionOverwritesInteractionRestriction() {
		// given
		val january = integerRange("JanuaryOnly", 1, 1)
		amlModel.dateText.valueSpaceAssignments += amlModel.monthVariable.restrictTo(january)
		amlModel.enterMonthAndYear.valueSpaceAssignments += amlModel.monthVariable.restrictTo(amlModel.validMonth)

		// when + then
		monthValue.valueSpace.assertPresent.assertSame(january)
	}

	@Test
	def void restrictionOnComponent() {
		// given
		val allowedSeconds = integerRange("ValidWaitTime", 1, 20)
		amlModel.myDialog.valueSpaceAssignments += amlModel.secondsVariable.restrictTo(allowedSeconds)

		// when + then
		secondsValue.valueSpace.assertPresent.assertSame(allowedSeconds)
	}

}
