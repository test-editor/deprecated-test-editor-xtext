/*******************************************************************************
 * Copyright (c) 2012 - 2018 Signal Iduna Corporation and others.
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
package org.testeditor.rcp4.handlers;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.e4.ui.workbench.modeling.EPartService;
import org.eclipse.ui.IWorkbench;
import org.junit.Test;
import org.mockito.Mockito;
import org.testeditor.rcp4.Constants;

/**
 * Modul test for the Handler RestartAndResetUIHandler.
 *
 */
public class RestartAndResetUIHandlerTest {

	/**
	 * Tests the logic of the handler.
	 * 
	 * @throws Exception
	 *             on test failure
	 */
	@Test
	public void testResetAndRestart() throws Exception {
		RestartAndResetUIHandler handler = new RestartAndResetUIHandler();
		IWorkbench workbench = mock(IWorkbench.class);
		EPartService partService = mock(EPartService.class);
		IEclipsePreferences prefs = mock(IEclipsePreferences.class);
		handler.resetAndRestart(workbench, null, partService, prefs);
		verify(workbench, Mockito.atLeastOnce()).restart();
		verify(partService, Mockito.atLeastOnce()).saveAll(true);
		verify(prefs, Mockito.atLeastOnce()).put(Constants.RESET_APP_PROPERTY, "true");
		verify(prefs, Mockito.atLeastOnce()).flush();
	}

}
