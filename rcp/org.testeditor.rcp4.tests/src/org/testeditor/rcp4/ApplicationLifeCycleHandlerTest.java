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
package org.testeditor.rcp4;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.e4.ui.workbench.IWorkbench;
import org.junit.Before;
import org.junit.Test;

/**
 * Module tests for ApplicationLifeCycleHandler
 *
 */
public class ApplicationLifeCycleHandlerTest {

	private ApplicationLifeCycleHandler handler;
	private IEclipsePreferences prefs;

	/**
	 * Prepare the system for test.
	 */
	@Before
	public void setup() {
		handler = new ApplicationLifeCycleHandler();
		prefs = mock(IEclipsePreferences.class);
		System.getProperties().put(IWorkbench.CLEAR_PERSISTED_STATE, "false");
	}

	/**
	 * Tests that an user interaction is detected and the system property is
	 * set.
	 * 
	 * @throws Exception
	 *             on test failure.
	 */
	@Test
	public void testRestUIModelAfterUserInteraction() throws Exception {
		when(prefs.get(Constants.TE_WS_VERSION_ID, "UNKNOWN")).thenReturn(Constants.TE_WS_VERSION);
		when(prefs.getBoolean(Constants.RESET_APP_PROPERTY, false)).thenReturn(true);
		handler.handlePossibleUIModelReset(null, prefs);
		assertEquals("true", System.getProperties().get(IWorkbench.CLEAR_PERSISTED_STATE));
	}

	/**
	 * 
	 * @throws Exception
	 *             on test failure.
	 */
	@Test
	public void testRestUIModelAfterVersionSwitch() throws Exception {
		when(prefs.get(Constants.TE_WS_VERSION_ID, "UNKNOWN")).thenReturn("1.0");
		when(prefs.getBoolean(Constants.RESET_APP_PROPERTY, false)).thenReturn(false);
		handler.handlePossibleUIModelReset(null, prefs);
		assertEquals("true", System.getProperties().get(IWorkbench.CLEAR_PERSISTED_STATE));
	}

	/**
	 * Tests that no clear persisted state is set on no user reset request and
	 * that the ws version matches the te version.
	 * 
	 * @throws Exception
	 *             on test failure.
	 */
	@Test
	public void testNoResonForRestUIModel() throws Exception {
		when(prefs.get(Constants.TE_WS_VERSION_ID, "UNKNOWN")).thenReturn(Constants.TE_WS_VERSION);
		when(prefs.getBoolean(Constants.RESET_APP_PROPERTY, false)).thenReturn(false);
		handler.handlePossibleUIModelReset(null, prefs);
		assertEquals("false", System.getProperties().get(IWorkbench.CLEAR_PERSISTED_STATE));
	}

}
