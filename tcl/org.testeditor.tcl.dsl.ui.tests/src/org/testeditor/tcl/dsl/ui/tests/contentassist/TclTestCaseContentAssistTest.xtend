package org.testeditor.tcl.dsl.ui.tests.contentassist

import org.junit.Ignore
import org.junit.Test

@Ignore("This test works from the IDE but does not work from Maven / Tycho.") // TODO check with Xtext 2.12
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
		builder.assertTextAtCursorPosition("Mask", "config", "Cleanup", "Setup", "* ", "implements")
	}

	@Test
	def void testEmptyStepWithSpec() {
		newBuilder.append('''
			package test
			# Test implements Target
			
			Mask: SomeMask
		''').assertTextAtCursorPosition("Mask", "config", "Cleanup", "Setup", "* ")
	}

}
