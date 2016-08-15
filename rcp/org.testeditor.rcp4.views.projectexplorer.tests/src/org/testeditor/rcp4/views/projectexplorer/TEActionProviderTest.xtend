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
package org.testeditor.rcp4.views.projectexplorer

import org.eclipse.jface.action.ActionContributionItem
import org.eclipse.jface.action.IMenuManager
import org.junit.Test
import org.testeditor.dsl.common.testing.AbstractTest

import static org.mockito.Mockito.*
import org.eclipse.jface.action.IAction

class TEActionProviderTest extends AbstractTest {
	
	@Test
	def void testFixMenu() {
		// given
		val actionProvider = new TEActionProvider()
		val menuManager = mock(IMenuManager)
		val con = mock(ActionContributionItem)
		when(menuManager.find("org.eclipse.ui.RenameResourceAction")).thenReturn(con)
		when(menuManager.remove(con)).thenReturn(null)
		
		// when
		actionProvider.fillContextMenu(menuManager)
		
		// then
		verify(menuManager).appendToGroup(any(String),any(IAction))
	}
	
}
