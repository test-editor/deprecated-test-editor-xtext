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
package org.testeditor.rcp4.handlers;

import javax.inject.Named;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.e4.core.di.annotations.Execute;
import org.eclipse.e4.core.di.extensions.Preference;
import org.eclipse.e4.ui.services.IServiceConstants;
import org.eclipse.swt.widgets.Shell;
import org.testeditor.rcp4.Constants;
import org.testeditor.rcp4.dialogs.NetworkConnectionSettingDialog;

public class OpenNetworkConfigurationHandler {

	@Execute
	public void execute(@Named(IServiceConstants.ACTIVE_SHELL) Shell shell,
			@Preference(nodePath = Constants.CONFIGURATION_STORE) IEclipsePreferences prefs) {
		new NetworkConnectionSettingDialog(shell, prefs).open();
	}
}
