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

import org.junit.Test
import org.mockito.Mockito
import org.eclipse.swt.widgets.List

/**
 * Logic tests for TestProjectConfigurationWizardPage.
 */
class TestProjectConfigurationWizardPageTest {
	
	/**
	 * Tests the Selection Listener to move the content from one list to the other list.
	 */
	@Test
	def testUpdateListWithMoveListener() {
		//given
		val source = Mockito.mock(List)
		val dest = Mockito.mock(List)
		Mockito.when(source.selection).thenReturn(#["MyFixture"])
		val out = new TestProjectConfigurationWizardPage("").getMoveListener(source,dest)
		
		//when
		out.widgetSelected(null);
		
		//then
		Mockito.verify(dest).add("MyFixture")
		Mockito.verify(source).remove("MyFixture")
	}
	
}
