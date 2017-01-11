package org.testeditor.aml.dsl.tests.formatter

import org.junit.Test

class ValueSpaceAssignmentFormatterTest extends AbstractFormatterTest {

	@Test
	def void formatNewSyntax() {
		assertFormatted[
			expectation = '''
				interaction type Interaction {
					restrict x to y
				}
			'''
			toBeFormatted = '''
				interaction type Interaction {restrict       x    to
				   y
				}
			'''
		]
	}

	@Test
	def void formatOldSyntax() {
		assertFormatted[
			expectation = '''
				interaction type Interaction {
					x restrict to y
				}
			'''
			toBeFormatted = '''
				interaction type Interaction {x      restrict  to
				   y
				}
			'''
		]
	}

}
