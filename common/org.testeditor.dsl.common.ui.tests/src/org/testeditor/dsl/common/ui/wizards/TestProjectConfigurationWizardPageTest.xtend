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
package org.testeditor.dsl.common.ui.wizards

import org.eclipse.swt.widgets.List
import org.junit.Test

import static extension org.mockito.Mockito.*

/**
 * Logic tests for TestProjectConfigurationWizardPage.
 */
class TestProjectConfigurationWizardPageTest {

	/**
	 * Tests the Selection Listener to move the content from one list to the other list.
	 */
	@Test
	def void testUpdateListWithMoveListener() {
		// given
		val source = List.mock
		val dest = List.mock
		when(source.selection).thenReturn(#["MyFixture"])

		// when
		new TestProjectConfigurationWizardPage("").moveSelection(source, dest)

		// then
		verify(dest).add("MyFixture")
		verify(source).remove("MyFixture")
	}

}
