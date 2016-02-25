package org.testeditor.rcp4;

import org.apache.log4j.Logger
import org.apache.log4j.PropertyConfigurator
import org.eclipse.equinox.app.IApplication
import org.eclipse.equinox.app.IApplicationContext
import org.eclipse.ui.PlatformUI

/**
 * This class controls all aspects of the application's execution
 */
public class Application implements IApplication {
	val logger=Logger.getLogger(Application)

	/* (non-Javadoc)
	 * @see IApplication#start(org.eclipse.equinox.app.IApplicationContext)
	 */
	override start(IApplicationContext context) {
		PropertyConfigurator.configure(Application.classLoader.getResource("log4j.properties"));
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
