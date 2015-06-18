package org.testeditor.aml.dsl.tests.formatter

import org.junit.Ignore
import org.junit.Test
import org.testeditor.aml.model.Component

/**
 * Formatting tests specific to {@link Component}.
 */
class ComponentFormatterTest extends AbstractFormatterTest {
	
	@Test @Ignore
	def void formatInheritance() {
		assertFormatted[
			expectation = '''
				component MyWizard is Dialog includes DefaultDialog
				
				abstract component DefaultDialog is Dialog
				
				component type Tree
				component type Composite
			'''
			toBeFormatted = '''
				component MyWizard   is Dialog	includes 	DefaultDialog
				  abstract	component  DefaultDialog is Dialog
				component type Dialog
			'''
		]
	}
	
}