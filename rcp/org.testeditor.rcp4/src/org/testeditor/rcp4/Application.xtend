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

import java.io.IOException
import java.io.PrintStream
import org.eclipse.equinox.app.IApplication
import org.eclipse.equinox.app.IApplicationContext
import org.eclipse.jface.dialogs.MessageDialog
import org.eclipse.swt.widgets.Display
import org.eclipse.swt.widgets.Shell
import org.eclipse.ui.PlatformUI
import org.slf4j.LoggerFactory
import org.testeditor.logging.LoggerStream

/**
 * This class controls all aspects of the application's execution
 */
public class Application implements IApplication {
	static val logger = LoggerFactory.getLogger(Application)

	/* (non-Javadoc)
	 * @see IApplication#start(org.eclipse.equinox.app.IApplicationContext)
	 */
	override start(IApplicationContext context) {
		captureConsoleoutputIntoLogger
		logger.info("STARTING APPLICATION ..")
		val display = PlatformUI.createDisplay
		try {
			val returnCode = PlatformUI.createAndRunWorkbench(display, new ApplicationWorkbenchAdvisor)
			if (returnCode == PlatformUI.RETURN_RESTART) {
				return IApplication.EXIT_RESTART
			}
			if (!jdkUsed) {
				if (!continueWithConfigureationMissmatch(
					"Application is started with a jre. For test-execution a jdk is needed. Start anyway?")) {
					return IApplication.EXIT_OK
				}
			}
			return IApplication.EXIT_OK
		} finally {
			display.dispose
		}
	}

	def continueWithConfigureationMissmatch(String message) {
		return new MessageDialog(new Shell(new Display()), "Conifguration-Missmatch", null, message,
			MessageDialog.ERROR, #{"Yes", "No"}, 0).open() > 0
	}

	def boolean isJdkUsed() {
		try {
			Runtime.getRuntime().exec(#{"javac"})
		} catch (IOException e) {
			return false
		}
		return true
	}

	/* (non-Javadoc)
	 * @see IApplication#stop()
	 */
	override stop() {
		logger.info("STOPPING APPLICATION ..")
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

	/** 
	 * Logs all output that is logged (written) to System.out and System.err through
	 * the logger of this class. This allows the unification of eclipse rcp messages 
	 * (written to the console through call parameter '-consoleLog') and the log output
	 * of the test editor itself (through log4j). Configuration of log4j will then apply
	 * to messages from eclipse and the test editor (e.g. redirection to a file). 
	 */
	private def void captureConsoleoutputIntoLogger() {
		System.setOut(new PrintStream(new LoggerStream(logger, System.out)))
		System.setErr(new PrintStream(new LoggerStream(logger, System.err)))
	}

}
