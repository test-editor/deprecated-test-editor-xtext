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

import javax.inject.Named;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.e4.core.di.extensions.Preference;
import org.eclipse.e4.ui.services.IServiceConstants;
import org.eclipse.e4.ui.workbench.IWorkbench;
import org.eclipse.e4.ui.workbench.lifecycle.PostContextCreate;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.osgi.service.prefs.BackingStoreException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testeditor.rcp4.dialogs.NetworkConnectionSettingDialog;

/**
 * 
 * Handle Events on the Application Lifecycle.
 * 
 */
public class ApplicationLifeCycleHandler {

	private static final Logger logger = LoggerFactory.getLogger(ApplicationLifeCycleHandler.class);

	/**
	 * Checks the state of the Test-Editor Envirnoment like internet connection
	 * and ui model migration.
	 * 
	 * @param shell
	 * @param prefs
	 * @throws BackingStoreException
	 */
	@PostContextCreate
	public void checkTestEditorState(@Named(IServiceConstants.ACTIVE_SHELL) Shell shell,
			@Preference(nodePath = Constants.CONFIGURATION_STORE) IEclipsePreferences prefs)
			throws BackingStoreException {
		handlePossibleUIModelReset(shell, prefs);
		checkInternetConnection(shell, prefs);
	}

	/**
	 * Check the internet connection and opens a config page on problems with
	 * the internet connection.
	 * 
	 * @param shell
	 *            used to open a connection dialog.
	 * @param prefs
	 */
	private void checkInternetConnection(Shell shell, IEclipsePreferences prefs) {
		NetworkConnectionSettingDialog connectionSettingDialog = new NetworkConnectionSettingDialog(shell, prefs);
		if (!connectionSettingDialog.isInternetAvailable(false)) {
			connectionSettingDialog.open();
		}
	}

	/**
	 * Verifies if there is a reason to drop existing ui model states in the
	 * workspace. Reasons to drop the ui models are:
	 * 
	 * <ul>
	 * <li>User request</li>
	 * <li>New version of the Test-Editor</li>
	 * </ul>
	 * 
	 * @throws BackingStoreException
	 *             on problems storing preferences.
	 */
	public void handlePossibleUIModelReset(@Named(IServiceConstants.ACTIVE_SHELL) Shell shell,
			@Preference(nodePath = Constants.CONFIGURATION_STORE) IEclipsePreferences prefs)
			throws BackingStoreException {
		if (isUIModelResetNeeded(shell, prefs)) {
			System.getProperties().put(IWorkbench.CLEAR_PERSISTED_STATE, "true");
			prefs.put(Constants.TE_WS_VERSION_ID, Constants.TE_WS_VERSION);
			prefs.put(Constants.RESET_APP_PROPERTY, "false");
			prefs.flush();
			logger.info("Reset of ui model done.");
		}
	}

	/**
	 * Checks if it is useful to reset the ui model.
	 * 
	 * @param shell
	 * @param prefs
	 * 
	 * @return true if the Test-Editor workspace is older than the current te
	 *         version or the user requested it.
	 */
	private boolean isUIModelResetNeeded(Shell shell, IEclipsePreferences prefs) {
		if (!prefs.get(Constants.TE_WS_VERSION_ID, "UNKNOWN").equals(Constants.TE_WS_VERSION)) {
			if (shell != null) {
				boolean confirm = MessageDialog.openConfirm(shell, "Migrate TE",
						"Using the new features of this Test-Editor version, needs to reset the UI State of the Application in this workspace.");
				return confirm;
			}
			return true;
		}
		return prefs.getBoolean(Constants.RESET_APP_PROPERTY, false);
	}

}
