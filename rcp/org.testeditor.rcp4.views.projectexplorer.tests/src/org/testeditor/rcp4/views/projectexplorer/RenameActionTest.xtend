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

import org.testeditor.dsl.common.testing.AbstractTest
import static org.mockito.Mockito.*
import org.junit.Test
import org.eclipse.jface.action.IAction
import org.eclipse.jface.viewers.ISelectionProvider
import org.junit.Before
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.core.resources.IFolder
import org.eclipse.core.resources.IFile
import org.eclipse.ui.IWorkbenchPage

class RenameActionTest extends AbstractTest {

	IAction reanemAction

	RenameAction action
	
	IStructuredSelection selection

	@Before
	def void setUp() {
		reanemAction = mock(IAction)
		val provider = mock(ISelectionProvider)
		val page = mock(IWorkbenchPage)
		selection = mock(IStructuredSelection)
		when(provider.selection).thenReturn(selection)
		action = new RenameAction(reanemAction, provider, page)
	}

	@Test
	def void testExecuteOnFolder() {
		// given
		val folder = mock(IFolder)
		when(selection.firstElement).thenReturn(folder)
		
		// when
		action.run
		
		// then
		verify(reanemAction, times(1)).run
	}

	@Test
	def void testExecuteOnNonTestFile() {
		// given
		val file = mock(IFile)
		when(file.fileExtension).thenReturn("aml")
		when(selection.firstElement).thenReturn(file)

		// when
		action.run
		
		// then
		verify(reanemAction, times(1)).run
	}

}
