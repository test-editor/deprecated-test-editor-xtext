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