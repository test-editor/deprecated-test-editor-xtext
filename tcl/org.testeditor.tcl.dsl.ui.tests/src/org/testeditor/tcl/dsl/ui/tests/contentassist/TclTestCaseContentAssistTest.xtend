package org.testeditor.tcl.dsl.ui.tests.contentassist

import org.junit.Test

class TclTestCaseContentAssistTest extends AbstractTclContentAssistTest {

	@Test
	def void testEmptyStepWithoutSpec() {
		// given
		val builder = newBuilder.append('''
			package test
			# Test
			
			Mask: SomeMask
		''')

		// expect
		builder.assertTextAtCursorPosition("Mask", "* ", "implements")
	}

	@Test
	def void testEmptyStepWithSpec() {
		newBuilder.append('''
			package test
			# Test implements Target
			
			Mask: SomeMask
		''').assertTextAtCursorPosition("Mask", "* ")
	}

}
