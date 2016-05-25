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
package org.testeditor.rcp4.dialogs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.swt.widgets.Shell;
import org.junit.Test;

/**
 * Modultest for NetworkConnectionSettingDialog.
 *
 */
public class NetworkConnectionSettingDialogTest {

	@Test
	public void testWorkOfflineSetting() throws Exception {
		// Given
		Shell parentShell = mock(Shell.class);
		IEclipsePreferences prefs = mock(IEclipsePreferences.class);
		when(prefs.getBoolean("workOffline", false)).thenReturn(true);
		NetworkConnectionSettingDialog dialog = new NetworkConnectionSettingDialog(parentShell, prefs);
		// when & then
		assertTrue(dialog.isInternetAvailable(false));
	}

	@Test
	public void testNonOverrideExistingSetting() throws Exception {
		// given
		Shell parentShell = mock(Shell.class);
		IEclipsePreferences prefs = getPrefsMockWithProxy();
		System.setProperty("http.proxyHost", "myProxy");
		NetworkConnectionSettingDialog dialog = new NetworkConnectionSettingDialog(parentShell, prefs);
		// when
		assertTrue(dialog.isInternetAvailable(false));
		// then
		assertEquals("myProxy", System.getProperty("http.proxyHost"));
	}

	private IEclipsePreferences getPrefsMockWithProxy() {
		IEclipsePreferences prefs = mock(IEclipsePreferences.class);
		when(prefs.get("http.proxyHost", "")).thenReturn("proxy");
		when(prefs.get("http.proxyPort", "")).thenReturn("port");
		when(prefs.get("http.proxyUser", "")).thenReturn("user");
		when(prefs.get("http.proxyPassword", "")).thenReturn("pwd");
		when(prefs.get("http.nonProxyHosts", "")).thenReturn("host");
		when(prefs.get("TE.MAVENSETTINGSPATH", "")).thenReturn("");
		return prefs;
	}

	@Test
	public void testForceUpdate() throws Exception {
		// given
		Shell parentShell = mock(Shell.class);
		IEclipsePreferences prefs = getPrefsMockWithProxy();
		when(prefs.get("http.proxyHost", "")).thenReturn("proxy");
		System.setProperty("http.proxyHost", "myProxy");
		NetworkConnectionSettingDialog dialog = new NetworkConnectionSettingDialog(parentShell, prefs);
		// when
		assertTrue(dialog.isInternetAvailable(true));
		// then
		assertEquals("proxy", System.getProperty("http.proxyHost"));
	}

}
