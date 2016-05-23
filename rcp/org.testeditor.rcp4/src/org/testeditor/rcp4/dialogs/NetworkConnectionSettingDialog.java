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
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
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

	private static final String PATH_TO_MAVENSETTINGS = "MAVENSETTINGSPATH";
	private static final String PROXY_HOST = "http.proxyHost";
	private static final String WORKOFFLINE = "workOffline";
	private static final String PROXY_PORT = "http.proxyPor";
	private static final String PROXY_USER = "http.proxyUser";
	private static final String PROXY_PWD = "http.proxyPassword";

	private Label internetState;
	private Text pathToMavenSettings;
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

	public NetworkConnectionSettingDialog(Shell parentShell, IEclipsePreferences prefs) {
		super(parentShell);
		this.prefs = prefs;
		workOffline = prefs.getBoolean(WORKOFFLINE, false);
		proxyHostSetting = prefs.get(PROXY_HOST, "");
		proxyPortSetting = prefs.get(PROXY_PORT, "");
		proxyUserSetting = prefs.get(PROXY_USER, "");
		proxyPwdSetting = prefs.get(PROXY_PWD, "");
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
		new Label(container, SWT.NORMAL).setText("Proxy port:");
		proxyPort = new Text(container, SWT.BORDER);
		proxyPort.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		proxyPort.setText(proxyPortSetting);
		new Label(container, SWT.NORMAL).setText("Proxy username:");
		proxyUser = new Text(container, SWT.BORDER);
		proxyUser.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		proxyUser.setText(proxyUserSetting);
		new Label(container, SWT.NORMAL).setText("Proxy password:");
		proxyPwd = new Text(container, SWT.BORDER);
		proxyPwd.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		proxyPwd.setText(proxyPwdSetting);
	}

	private void createMavenSettingsWidgets(final Composite container) {
		new Label(container, SWT.NORMAL).setText("Path to maven settings:");
		Composite mavenPathControl = new Composite(container, SWT.NORMAL);
		mavenPathControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		mavenPathControl.setLayout(new GridLayout(2, false));
		pathToMavenSettings = new Text(mavenPathControl, SWT.BORDER);
		pathToMavenSettings.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		pathToMavenSettings.setText(prefs.get(PATH_TO_MAVENSETTINGS, ""));
		Button fileSelection = new Button(mavenPathControl, SWT.BORDER);
		fileSelection.setText("...");
		fileSelection.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				FileDialog fileDialog = new FileDialog(container.getShell());
				fileDialog.setText("Select Maven settings");
				String path = fileDialog.open();
				if (path != null) {
					pathToMavenSettings.setText(path);
				}
			}
		});
	}

	private void updateNetworkState() {
		Runnable runnable = new Runnable() {

			@Override
			public void run() {
				if (isInternetAvailable()) {
					if (workOffline) {
						updateNetworkSateWidget("work offline", SWT.COLOR_DARK_YELLOW);
					} else {
						updateNetworkSateWidget("connected", SWT.COLOR_DARK_GREEN);
					}
				} else {
					updateNetworkSateWidget("diconnected", SWT.COLOR_RED);
				}
			}
		};
		new Thread(runnable).start();
	}

	protected void updateNetworkSateWidget(final String message, final int color) {
		Display.getDefault().asyncExec(new Runnable() {

			@Override
			public void run() {
				internetState.setText(message);
				internetState.setForeground(Display.getDefault().getSystemColor(color));
				internetState.getParent().layout(true);
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
		return new Point(500, 350);
	}

	@Override
	protected void okPressed() {
		prefs.putBoolean(WORKOFFLINE, workOffline);
		prefs.put(PATH_TO_MAVENSETTINGS, pathToMavenSettings.getText());
		prefs.put(PROXY_HOST, proxyHost.getText());
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
	public boolean isInternetAvailable() {
		if (workOffline) {
			return true;
		}
		if (System.getProperty(PROXY_HOST) == null) {
			if (proxyHostSetting.length() > 0) {
				System.setProperty(PROXY_HOST, proxyHostSetting);
				System.setProperty(PROXY_PORT, proxyPortSetting);
				System.setProperty(PROXY_USER, proxyUserSetting);
				System.setProperty(PROXY_PWD, proxyPwdSetting);
			}
		}
		try (Socket socket = new Socket()) {
			InetAddress inetAddress = InetAddress.getByName("www.google.com");
			InetSocketAddress adr = new InetSocketAddress(inetAddress, 80);
			socket.connect(adr, 500);
			return true;
		} catch (IOException e) {
			return false;
		}
	}

}
