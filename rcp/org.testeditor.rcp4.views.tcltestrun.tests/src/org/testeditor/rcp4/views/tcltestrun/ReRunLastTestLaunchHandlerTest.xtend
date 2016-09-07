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
package org.testeditor.rcp4.views.tcltestrun

import org.testeditor.dsl.common.testing.AbstractTest
import org.junit.Test
import org.eclipse.e4.core.contexts.EclipseContextFactory
import org.eclipse.e4.core.contexts.ContextInjectionFactory

import static extension org.mockito.Mockito.*

class ReRunLastTestLaunchHandlerTest extends AbstractTest {
	
	
	@Test
	def void testCanExecute() {
		// given
		val ctx = EclipseContextFactory.create
		val handler = ContextInjectionFactory.make(ReRunLastTestLaunchHandler,ctx)
		
		// when
		val canExecuteEmpty = handler.canExecute(ctx)
		ctx.set(LastTestLaunchInformation, new LastTestLaunchInformation)
		val canExecuteWithLastLaunch = handler.canExecute(ctx)
		
		// then
		assertFalse(canExecuteEmpty)
		assertTrue(canExecuteWithLastLaunch)
	}

	@Test
	def void testExecute() {
		// given
		val ctx = EclipseContextFactory.create
		val tclLauncherUi = TclLauncherUi.mock
		ctx.set(TclLauncherUi, tclLauncherUi)
		val testList = #["test1","test2"]
		val lastlaunch = new LastTestLaunchInformation
		lastlaunch => [
			testCasesCommaList = testList
		]
		ctx.set(LastTestLaunchInformation, lastlaunch)
		val handler = ContextInjectionFactory.make(ReRunLastTestLaunchHandler,ctx)

		//when
		handler.reLaunchLastTestLaunch(ctx)
		
		//then			
		verify(tclLauncherUi).launchTest(testList,null,null,null)	
	}
	
}