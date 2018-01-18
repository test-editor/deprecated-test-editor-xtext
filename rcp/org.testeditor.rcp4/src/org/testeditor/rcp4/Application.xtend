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
package org.testeditor.rcp4;

import java.io.IOException
import java.io.PrintStream
import java.util.regex.Pattern
import org.eclipse.equinox.app.IApplication
import org.eclipse.equinox.app.IApplicationContext
import org.eclipse.jface.dialogs.MessageDialog
import org.eclipse.swt.widgets.Shell
import org.eclipse.ui.PlatformUI
import org.slf4j.LoggerFactory
import org.testeditor.dsl.common.util.OutputStreamCopyUtil
import org.testeditor.logging.LoggerStream

/**
 * This class controls all aspects of the application's execution
 */
public class Application implements IApplication {
	static val logger = LoggerFactory.getLogger(Application)
	val MIN_JAVA_MAJOR_VERSION = 1
	val MIN_JAVA_MINOR_VERSION = 8

	/* (non-Javadoc)
	 * @see IApplication#start(org.eclipse.equinox.app.IApplicationContext)
	 */
	override start(IApplicationContext context) {
		captureConsoleoutputIntoLogger
		logger.info("STARTING APPLICATION ..")
		val display = PlatformUI.createDisplay
		if (!isExpectedJavaEnv && !answerYesNoMessageDialog(new Shell(display),
			"Application is started with unexpected java environment. For test-execution a jdk 1.8 or later is needed. Start anyway?")) {
			logger.info("Application stopped since java compiler (jdk) not available")
			logger.info("STOPPING APPLICATION ..")
			return IApplication.EXIT_OK
		}
		try {
			val returnCode = PlatformUI.createAndRunWorkbench(display, new ApplicationWorkbenchAdvisor)
			if (returnCode == PlatformUI.RETURN_RESTART) {
				logger.info("RESTARTING APPLICATION ..")
				return IApplication.EXIT_RESTART
			}
			logger.info("STOPPING APPLICATION ..")
			return IApplication.EXIT_OK
		} finally {
			display.dispose
		}
	}

	def boolean answerYesNoMessageDialog(Shell shell, String message) {
		return new MessageDialog(shell, "Configuration-Mismatch", null, message, MessageDialog.ERROR, #{"Yes", "No"},
			0).open() > 0
	}

	def boolean isExpectedJavaEnv() {
		val javaHome = System.properties.get("java.home").toString
		val javaVersion = System.properties.get("java.version").toString
		logger.info("java.home = {}", javaHome)
		logger.info("java.version = {}", javaVersion)
		val versionPattern = Pattern.compile("^([1-9])\\.([0-9]+).*")
		val versionMatcher = versionPattern.matcher(javaVersion)
		if (!versionMatcher.matches) {
			return false
		}
		val major = Integer.parseInt(versionMatcher.group(1))
		val minor = Integer.parseInt(versionMatcher.group(2))
		if (major < MIN_JAVA_MAJOR_VERSION || (major == MIN_JAVA_MAJOR_VERSION && minor < MIN_JAVA_MINOR_VERSION)) {
			return false
		}

		try {
			val proc = Runtime.runtime.exec(#["javac", "-version"])
			val out = new PrintStream(new LoggerStream(logger))
			val err = new PrintStream(new LoggerStream(logger))
			val outputCopyThread = new OutputStreamCopyUtil(proc.inputStream, out)
			val errorCopyThread = new OutputStreamCopyUtil(proc.errorStream, err)
			outputCopyThread.start
			errorCopyThread.start
			proc.waitFor
			outputCopyThread.join
			errorCopyThread.join
		} catch (IOException e) {
			return false
		}
		return true
	}

	/* (non-Javadoc)
	 * @see IApplication#stop()
	 */
	override stop() {
		logger.info("EXECUTE APPLICATION STOP ..")
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
