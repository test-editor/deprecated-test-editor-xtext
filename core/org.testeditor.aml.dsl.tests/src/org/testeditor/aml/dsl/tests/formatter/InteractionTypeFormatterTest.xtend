package org.testeditor.aml.dsl.tests.formatter

import org.junit.Test

class InteractionTypeFormatterTest extends AbstractFormatterTest {

	@Test
	def void regressionInteractionType() {
		assertFormatted[
			expectation = '''
				interaction type Pruefe_Text_Nicht_In_Element {
					label = "Ist Text am Element nicht vorhanden"
					template = "überprüfe ob am Element" ${element} "der Text" ${} "nicht vorhanden ist"
				}
			'''

			toBeFormatted = '''
				interaction
				type Pruefe_Text_Nicht_In_Element {
					label = "Ist Text am Element nicht vorhanden" template = "überprüfe ob am Element" ${element} "der Text" ${} "nicht vorhanden ist"
				}
			'''
		]
	}

}