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

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;

import org.eclipse.core.net.proxy.IProxyData;
import org.eclipse.core.net.proxy.IProxyService;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.e4.core.contexts.IEclipseContext;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.osgi.service.prefs.BackingStoreException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testeditor.rcp4.ApplicationLifeCycleHandler;

/**
 * Dialog to manage network settings.
 *
 */
public class NetworkConnectionSettingDialog extends Dialog {

	private static final Logger logger = LoggerFactory.getLogger(ApplicationLifeCycleHandler.class);

	private static final String PATH_TO_MAVENSETTINGS = "TE.MAVENSETTINGSPATH";
	private static final String PROXY_HOST = "http.proxyHost";
	private static final String WORKOFFLINE = "workOffline";
	private static final String PROXY_PORT = "http.proxyPort";
	private static final String PROXY_USER = "http.proxyUser";
	private static final String PROXY_PWD = "http.proxyPassword";
	private static final String NON_PROXY_HOSTS = "http.nonProxyHosts";
	private static final String PROXY_HTTPS_HOST = "https.proxyHost";
	private static final String PROXY_HTTPS_PORT = "https.proxyPort";

	private Label internetState;
	private Text pathToMavenSettingsFile;
	private String pathToMavenSettingsFileSettings;
	private Text proxyHost;
	private Button offlineSwtich;
	private boolean workOffline;
	private String proxyHostSetting;
	private IEclipsePreferences prefs;
	private Text proxyPort;
	private String proxyPortSetting;
	private Text proxyUser;
	private String proxyUserSetting;
	private Text proxyPwd;
	private String proxyPwdSetting;
	private Text noProxyHosts;

	private String noProxyHostsSetting;

	private IEclipseContext context;

	public NetworkConnectionSettingDialog(Shell parentShell, IEclipsePreferences prefs, IEclipseContext context) {
		super(parentShell);
		this.prefs = prefs;
		this.context = context;
		workOffline = prefs.getBoolean(WORKOFFLINE, false);
		proxyHostSetting = prefs.get(PROXY_HOST, "");
		proxyPortSetting = prefs.get(PROXY_PORT, "");
		proxyUserSetting = prefs.get(PROXY_USER, "");
		proxyPwdSetting = prefs.get(PROXY_PWD, "");
		noProxyHostsSetting = prefs.get(NON_PROXY_HOSTS, "");
		pathToMavenSettingsFileSettings = prefs.get(PATH_TO_MAVENSETTINGS, "");
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		Composite container = (Composite) super.createDialogArea(parent);
		container.setLayout(new GridLayout(2, false));
		new Label(container, SWT.NORMAL).setText("Connectivity state: ");
		internetState = new Label(container, SWT.NORMAL);
		updateNetworkState();
		createMavenSettingsWidgets(container);
		createProxySettingsWidgets(container);
		offlineSwtich = new Button(container, SWT.CHECK);
		offlineSwtich.setText("Work offline");
		offlineSwtich.setSelection(workOffline);
		offlineSwtich.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				workOffline = offlineSwtich.getSelection();
				updateNetworkState();
			}
		});
		return container;
	}

	private void createProxySettingsWidgets(Composite container) {
		new Label(container, SWT.NORMAL).setText("Proxy host:");
		proxyHost = new Text(container, SWT.BORDER);
		proxyHost.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		proxyHost.setText(proxyHostSetting);
		proxyHost.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {
				proxyHostSetting = proxyHost.getText();
				updateNetworkState();
			}
		});
		new Label(container, SWT.NORMAL).setText("Proxy port:");
		proxyPort = new Text(container, SWT.BORDER);
		proxyPort.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		proxyPort.setText(proxyPortSetting);
		proxyPort.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {
				proxyPortSetting = proxyPort.getText();
				updateNetworkState();
			}
		});
		new Label(container, SWT.NORMAL).setText("Proxy username:");
		proxyUser = new Text(container, SWT.BORDER);
		proxyUser.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		proxyUser.setText(proxyUserSetting);
		proxyUser.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {
				proxyUserSetting = proxyUser.getText();
				updateNetworkState();
			}
		});
		new Label(container, SWT.NORMAL).setText("Proxy password:");
		proxyPwd = new Text(container, SWT.BORDER);
		proxyPwd.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		proxyPwd.setText(proxyPwdSetting);
		proxyPwd.setEchoChar('*');
		proxyPwd.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {
				proxyPwdSetting = proxyPwd.getText();
				updateNetworkState();
			}
		});
		new Label(container, SWT.NORMAL).setText("No Proxy for:");
		noProxyHosts = new Text(container, SWT.BORDER);
		noProxyHosts.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		noProxyHosts.setText(noProxyHostsSetting);
		noProxyHosts.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {
				noProxyHostsSetting = noProxyHosts.getText();
				updateNetworkState();
			}
		});
	}

	private void createMavenSettingsWidgets(final Composite container) {
		new Label(container, SWT.NORMAL).setText("Path to maven settings:");
		Composite mavenPathControl = new Composite(container, SWT.NORMAL);
		mavenPathControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		mavenPathControl.setLayout(new GridLayout(2, false));
		pathToMavenSettingsFile = new Text(mavenPathControl, SWT.BORDER);
		pathToMavenSettingsFile.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		pathToMavenSettingsFile.setText(pathToMavenSettingsFileSettings);
		pathToMavenSettingsFile.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {
				pathToMavenSettingsFileSettings = pathToMavenSettingsFile.getText();
			}
		});
		Button fileSelection = new Button(mavenPathControl, SWT.BORDER);
		fileSelection.setText("...");
		fileSelection.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				FileDialog fileDialog = new FileDialog(container.getShell());
				fileDialog.setText("Select Maven settings");
				String path = fileDialog.open();
				if (path != null) {
					pathToMavenSettingsFile.setText(path);
				}
			}
		});
	}

	protected void updateNetworkState() {
		Display.getDefault().asyncExec(new Runnable() {

			@Override
			public void run() {
				if (isInternetAvailable(true)) {
					if (workOffline) {
						updateUI("work offline", SWT.COLOR_DARK_YELLOW);
					} else {
						updateUI("connected", SWT.COLOR_DARK_GREEN);
					}
				} else {
					updateUI("diconnected", SWT.COLOR_RED);
				}

			}

			public void updateUI(String message, int color) {
				if (!internetState.isDisposed()) {
					internetState.setText(message);
					internetState.setForeground(Display.getDefault().getSystemColor(color));
					internetState.getParent().layout(true);
				}
			}
		});
	}

	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText("Network configuration dialog");
	}

	@Override
	protected Point getInitialSize() {
		return new Point(500, 380);
	}

	@Override
	protected void okPressed() {
		prefs.putBoolean(WORKOFFLINE, workOffline);
		prefs.put(PATH_TO_MAVENSETTINGS, pathToMavenSettingsFileSettings);
		prefs.put(PROXY_HOST, proxyHostSetting);
		prefs.put(PROXY_PORT, proxyPortSetting);
		prefs.put(PROXY_USER, proxyUserSetting);
		prefs.put(PROXY_PWD, proxyPwdSetting);
		prefs.put(NON_PROXY_HOSTS, noProxyHostsSetting);
		try {
			prefs.flush();
		} catch (BackingStoreException e) {
			logger.error("Error storring network settings", e);
		}
		super.okPressed();
	}

	/**
	 * Check if the TE can connect to the internet.
	 * 
	 * @return if it is possible to reach internet resources.
	 */
	public boolean isInternetAvailable(boolean updateSessings) {
		if (workOffline) {
			return true;
		}
		if (System.getProperty(PROXY_HOST) == null | updateSessings) {
			if (proxyHostSetting.length() > 0) {
				IProxyService proxyService = context.get(IProxyService.class);
				proxyService.setProxiesEnabled(true);
				proxyService.setSystemProxiesEnabled(true);
				IProxyData proxyData = proxyService.getProxyData(IProxyData.HTTP_PROXY_TYPE);
				IProxyData httpsProxyData = proxyService.getProxyData(IProxyData.HTTPS_PROXY_TYPE);
				proxyData.setHost(proxyHostSetting);
				httpsProxyData.setHost(proxyHostSetting);
				if (proxyPortSetting.length() > 0) {
					int port = Integer.parseInt(proxyPortSetting);
					proxyData.setPort(port);
					httpsProxyData.setPort(port);
				}
				proxyData.setUserid(proxyUserSetting);
				proxyData.setPassword(proxyPwdSetting);
				httpsProxyData.setUserid(proxyUserSetting);
				httpsProxyData.setPassword(proxyPwdSetting);
				try {
					if (noProxyHostsSetting.length() > 0) {
						proxyService.setNonProxiedHosts(noProxyHostsSetting.split(","));
					}
				} catch (CoreException e) {
					e.printStackTrace();
				}
				System.setProperty(PROXY_HOST, proxyHostSetting);
				System.setProperty(PROXY_HTTPS_HOST, proxyHostSetting);
				System.setProperty(PROXY_PORT, proxyPortSetting);
				System.setProperty(PROXY_HTTPS_PORT, proxyPortSetting);
				System.setProperty(PROXY_USER, proxyUserSetting);
				System.setProperty(PROXY_PWD, proxyPwdSetting);
				System.setProperty(NON_PROXY_HOSTS, noProxyHostsSetting);
				if (pathToMavenSettingsFileSettings.length() > 0) {
					System.setProperty(PATH_TO_MAVENSETTINGS, pathToMavenSettingsFileSettings);
				}
			}
		}
		try {
			URL url = new URL("http://www.google.com");
			URLConnection openConnection = url.openConnection();
			openConnection.connect();
			return true;
		} catch (IOException e) {
			return false;
		}
	}

}
