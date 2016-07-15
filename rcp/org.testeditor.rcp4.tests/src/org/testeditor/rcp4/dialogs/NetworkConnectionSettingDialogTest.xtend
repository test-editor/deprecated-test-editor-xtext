/** 
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 */
package org.testeditor.rcp4.dialogs

import static org.mockito.Mockito.mock
import static org.mockito.Mockito.when
import static java.nio.charset.StandardCharsets.*
import java.io.File
import org.eclipse.core.net.proxy.IProxyData
import org.eclipse.core.net.proxy.IProxyService
import org.eclipse.core.runtime.preferences.IEclipsePreferences
import org.eclipse.e4.core.contexts.IEclipseContext
import org.eclipse.swt.widgets.Shell
import org.junit.Test
import org.testeditor.dsl.common.testing.AbstractTest
import org.mockito.Mock
import com.google.common.io.Files
import org.junit.Rule
import org.junit.rules.TemporaryFolder

/** 
 * Modultest for NetworkConnectionSettingDialog.
 */
class NetworkConnectionSettingDialogTest extends AbstractTest {

	@Rule public TemporaryFolder tempFolder = new TemporaryFolder();

	@Mock Shell parentShell
	@Mock IEclipsePreferences prefs
	@Mock IEclipseContext context

	@Test def void testWorkOfflineSetting() throws Exception {
		// Given
		when(prefs.getBoolean("workOffline", false)).thenReturn(true)
		var NetworkConnectionSettingDialog dialog = new NetworkConnectionSettingDialog(parentShell, prefs, null)
		// when & then
		assertTrue(dialog.isInternetAvailable(false))
	}

	@Test def void testNonOverrideExistingSetting() throws Exception {
		// given
		initPrefsMockWithProxy()
		System.setProperty("http.proxyHost", "myProxy")
		var NetworkConnectionSettingDialog dialog = new NetworkConnectionSettingDialog(parentShell, prefs, null)
		// when
		assertTrue(dialog.isInternetAvailable(false))
		// then
		assertEquals("myProxy", System.getProperty("http.proxyHost"))
	}

	def private void initPrefsMockWithProxy() {
		when(prefs.get("http.proxyHost", "")).thenReturn("proxy")
		when(prefs.get("http.proxyPort", "")).thenReturn("80")
		when(prefs.get("http.proxyUser", "")).thenReturn("user")
		when(prefs.get("http.proxyPassword", "")).thenReturn("pwd")
		when(prefs.get("http.nonProxyHosts", "")).thenReturn("host")
		when(prefs.get("TE.MAVENSETTINGSPATH", "")).thenReturn("")
	}

	@Test def void testForceUpdate() throws Exception {
		// given
		initPrefsMockWithProxy()
		when(prefs.get("http.proxyHost", "")).thenReturn("proxy")
		System.setProperty("http.proxyHost", "myProxy")
		initContextWithProxyMock()
		var NetworkConnectionSettingDialog dialog = new NetworkConnectionSettingDialog(parentShell, prefs, context)
		// when
		assertTrue(dialog.isInternetAvailable(true))
		// then
		assertEquals("proxy", System.getProperty("http.proxyHost"))
	}

	@Test def void testExtractProxySettingsFromMavenFile() throws Exception {
		// Given
		initContextWithProxyMock()
		val NetworkConnectionSettingDialog dialog = new NetworkConnectionSettingDialog(parentShell, prefs, context)
		val File mavenSettingsFile = tempFolder.newFile("settings.xml")
		val content = '''
			<?xml version="1.0" encoding="UTF-8"?>
			<settings xmlns="http://maven.apache.org/SETTINGS/1.0.0"
				xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
				xsi:schemaLocation="http://maven.apache.org/SETTINGS/1.0.0
				https://maven.apache.org/xsd/settings-1.0.0.xsd">
				<proxies>
					<proxy>
						<id>myproxy</id>
						<active>true</active>
						<protocol>http</protocol>
						<host>proxy.somewhere.com</host>
						<port>8080</port>
						<username>proxyuser</username>
						<password>somepassword</password>
						<nonProxyHosts>*.google.com|ibiblio.org</nonProxyHosts>
					</proxy>
				</proxies>
			</settings>'''
		Files.write(content, mavenSettingsFile, UTF_8);

		// when
		dialog.extractProxyFromMavenSettings(mavenSettingsFile)
		dialog.isInternetAvailable(true)
		
		// then
		assertEquals("proxy.somewhere.com", System.getProperty("http.proxyHost"))
	}
	
	def initContextWithProxyMock() {
		var IProxyService proxyService = mock(IProxyService)
		var IProxyData proxyData = mock(IProxyData)
		when(proxyService.getProxyData(IProxyData.HTTP_PROXY_TYPE)).thenReturn(proxyData)
		when(proxyService.getProxyData(IProxyData.HTTPS_PROXY_TYPE)).thenReturn(proxyData)
		when(context.get(IProxyService)).thenReturn(proxyService)
	}
	
}
