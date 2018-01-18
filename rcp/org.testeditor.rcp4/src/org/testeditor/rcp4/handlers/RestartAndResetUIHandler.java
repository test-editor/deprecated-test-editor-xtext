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

import javax.inject.Named;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.e4.core.di.annotations.Execute;
import org.eclipse.e4.core.di.annotations.Optional;
import org.eclipse.e4.core.di.extensions.Preference;
import org.eclipse.e4.ui.services.IServiceConstants;
import org.eclipse.e4.ui.workbench.modeling.EPartService;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.osgi.service.prefs.BackingStoreException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testeditor.rcp4.Constants;

/**
 * 
 * Handler to send a restart command to the framework with a clear persistence
 * command.
 * 
 */
public class RestartAndResetUIHandler {

	private static final Logger logger = LoggerFactory.getLogger(RestartAndResetUIHandler.class);

	/**
	 * Restarts the Application to reset the ui model.
	 * 
	 * @param workbench
	 *            to be restarted.
	 * @param shell
	 *            activeShell to open a confirm Dialog.
	 * 
	 * @param translate
	 *            TestEditor Translation Service to translate the confirmation
	 *            dialog.
	 * @param partService
	 *            to store opened editor documents.
	 * 
	 * @param testEditorConfigService
	 *            service to store the reset application state on restart.
	 */
	@Execute
	public void resetAndRestart(@Optional IWorkbench workbench,
			@Optional @Named(IServiceConstants.ACTIVE_SHELL) Shell shell, EPartService partService,
			@Preference(nodePath = Constants.CONFIGURATION_STORE) IEclipsePreferences prefs) {
		if (shell != null) {
			if (!MessageDialog.openConfirm(shell, "Restart requested", "Restart the application to reset the UI.")) {
				return;
			}
		}
		try {
			prefs.put(Constants.RESET_APP_PROPERTY, "true");
			prefs.flush();
			partService.saveAll(true);
			workbench.restart();
		} catch (BackingStoreException e) {
			logger.error("Error storing the restart command. Restart aborted.", e);
			MessageDialog.openError(shell, "Error", "Unable to restart the application.");
		}
	}

}