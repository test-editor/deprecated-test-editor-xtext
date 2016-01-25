package org.testeditor.rcp4.product;

import org.eclipse.equinox.app.IApplication
import org.eclipse.equinox.app.IApplicationContext
import org.eclipse.ui.PlatformUI

/**
 * This class controls all aspects of the application's execution
 */
public class Application implements IApplication {

	/* (non-Javadoc)
	 * @see IApplication#start(org.eclipse.equinox.app.IApplicationContext)
	 */
	override start(IApplicationContext context) {
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
