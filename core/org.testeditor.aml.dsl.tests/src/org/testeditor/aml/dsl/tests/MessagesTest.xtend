package org.testeditor.aml.dsl.tests

import org.junit.Test
import org.testeditor.aml.dsl.Messages

/**
 * Test that the {@link Messages} class works properly.
 */
class MessagesTest extends AbstractTest {
	
	@Test
	def void testMessages() {
		val message = Messages.Validation_Component_Type_Missing
		if (message.nullOrEmpty) {
			fail("Could not load messages.properties")
		}
		if (message.contains("NLS missing message")) {
			fail("Could not load messages.properties")
		}
	}
	
}