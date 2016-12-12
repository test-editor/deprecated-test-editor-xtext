package org.testeditor.tcl.util

import javax.inject.Inject
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.ComponentElementType
import org.testeditor.aml.ComponentType
import org.testeditor.aml.InteractionType
import org.testeditor.aml.TemplateVariable
import org.testeditor.aml.ValueSpace
import org.testeditor.dsl.common.testing.DslParseHelper

@Accessors(PUBLIC_GETTER)
class ExampleAmlModel {

	AmlModel model

	ValueSpace validMonth
	ValueSpace validYear
	InteractionType enterMonthAndYear
	InteractionType wait
	ComponentElementType dateText
	ComponentType dialog
	Component myDialog
	ComponentElement date

	TemplateVariable monthVariable
	TemplateVariable yearVariable
	TemplateVariable secondsVariable

	@Inject
	new(DslParseHelper parserHelper, extension ValidationTestHelper validationHelper) {
		val aml = '''
			package com.example
			
			value-space validMonth = 1 .. 12
			value-space validYear = 1900 .. 2099
			
			interaction type enterMonthAndYear {
				template = "Enter month" ${month} "and year" ${year} "into" ${element}
			}
			
			interaction type wait {
				template = "Wait for" ${seconds} "seconds"
			}
			
			element type DateText {
				interactions = enterMonthAndYear
			}
			
			component type Dialog {
				interactions = wait
			}
			
			component MyDialog is Dialog {
				
				element Date is DateText
				
			}
		'''
		parserHelper.parseAml(aml) => [
			assertNoErrors
			this.model = it
			this.validMonth = valueSpaces.findFirst[name == "validMonth"]
			this.validYear = valueSpaces.findFirst[name == "validYear"]
			this.enterMonthAndYear = interactionTypes.findFirst[name == "enterMonthAndYear"]
			this.wait = interactionTypes.findFirst[name == "wait"]
			this.dateText = componentElementTypes.findFirst[name == "DateText"]
			this.dialog = componentTypes.findFirst[name == "Dialog"]
			this.myDialog = components.findFirst[name == "MyDialog"]
			this.date = myDialog.elements.findFirst[name == "Date"]

			this.monthVariable = enterMonthAndYear.template.contents.filter(TemplateVariable).findFirst[name == "month"]
			this.yearVariable = enterMonthAndYear.template.contents.filter(TemplateVariable).findFirst[name == "year"]
			this.secondsVariable = wait.template.contents.filter(TemplateVariable).findFirst[name == "seconds"]
		]
	}

}
