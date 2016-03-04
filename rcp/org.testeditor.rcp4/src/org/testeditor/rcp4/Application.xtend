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

import org.eclipse.equinox.app.IApplication
import org.eclipse.equinox.app.IApplicationContext
import org.eclipse.ui.PlatformUI
import org.slf4j.LoggerFactory

/**
 * This class controls all aspects of the application's execution
 */
public class Application implements IApplication {
	val logger=LoggerFactory.getLogger(Application)

	/* (non-Javadoc)
	 * @see IApplication#start(org.eclipse.equinox.app.IApplicationContext)
	 */
	override start(IApplicationContext context) {
		logger.info("STARTING APPLICATION ..");
		val display = PlatformUI.createDisplay
		try {
			val returnCode = PlatformUI.createAndRunWorkbench(display, new ApplicationWorkbenchAdvisor)
			if (returnCode == PlatformUI.RETURN_RESTART) {
				return IApplication.EXIT_RESTART
			} else {
				return IApplication.EXIT_OK
			}
		} finally {
			display.dispose
		}
	}

	/* (non-Javadoc)
	 * @see IApplication#stop()
	 */
	override stop() {
		logger.info("STOPPING APPLICATION ..");
		if (!PlatformUI.isWorkbenchRunning) {
			return
		}
		val workbench = PlatformUI.workbench
		val display = workbench.display
		display.syncExec [
			if (!display.isDisposed) {
				workbench.close
			}
		]
	}
}
