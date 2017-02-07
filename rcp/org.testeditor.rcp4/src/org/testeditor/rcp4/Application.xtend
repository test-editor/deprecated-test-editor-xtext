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
import java.util.Scanner
import java.io.PrintStream
import org.eclipse.equinox.app.IApplication
import org.eclipse.equinox.app.IApplicationContext
import org.eclipse.jface.dialogs.MessageDialog
import org.eclipse.swt.widgets.Display
import org.eclipse.swt.widgets.Shell
import org.eclipse.ui.PlatformUI
import org.gradle.internal.impldep.org.apache.commons.lang.SystemUtils
import org.slf4j.LoggerFactory
import org.testeditor.dsl.common.util.MavenExecutor
import org.testeditor.dsl.common.util.MavenExecutor.MavenVersionCheck
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
		if (!checkJdkIsUsed) {
			if (!continueWithConfigureationMissmatch(
				"Application is started with a jre. For test-execution a jdk is needed. Start anyway?")) {
				return IApplication.EXIT_OK
			}
		}

		val mavenVersionCheck = checkMavenVersion
		if (mavenVersionCheck === MavenVersionCheck.no_maven) {
			if (!continueWithConfigureationMissmatch(
				"No maven installation was found. Please install maven with minimum version " +
					MavenExecutor.MAVEN_MIMIMUM_MAJOR_VERSION + "." + MavenExecutor.MAVEN_MIMIMUM_MINOR_VERSION +
					" and set the variable " + MavenExecutor.TE_MAVEN_HOME +
					" to the path of the installation.\n\nStart without maven? Test execution with maven will not be possible")) {
						return IApplication.EXIT_OK
					}
				}
				if (mavenVersionCheck === MavenVersionCheck.wrong_version) {
					if (!continueWithConfigureationMissmatch(
						"Maven is not available in the needed version for test-execution. Start anyway?")) {
						return IApplication.EXIT_OK
					}
				}
				if (mavenVersionCheck === MavenVersionCheck.unknown_version) {
					if (!continueWithConfigureationMissmatch(
						"It was not possible to determine the current maven version. Testexecution may not be possible. Start anyway?")) {
						return IApplication.EXIT_OK
					}
					return IApplication.EXIT_OK
				}

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


			def continueWithConfigureationMissmatch(String message) {
				return new MessageDialog(new Shell(new Display()), "Conifguration-Missmatch", null, message,
					MessageDialog.ERROR, #{"Yes", "No"}, 0).open() > 0
			}

			def MavenVersionCheck checkMavenVersion() {
				val mavenCommand = MavenExecutor.getPathToMavenExecutable(SystemUtils.IS_OS_WINDOWS) 
				if (mavenCommand == "") {
					return MavenVersionCheck.no_maven
				}
				val command = newArrayList(MavenExecutor.getPathToMavenExecutable(SystemUtils.IS_OS_WINDOWS), "-v")

				val processBuilder = new ProcessBuilder()
				processBuilder.command(command)
				var Process process = null
				try {
					process = processBuilder.start()
				} catch (IOException e) {
					logger.error("could not start maven with command {}", command)
					return MavenVersionCheck.no_maven
				}

				val versionInfoScanner = new Scanner(process.getInputStream()).useDelimiter("\n")

				var String versionLine = null
				while (versionInfoScanner.hasNext) {
					val line = versionInfoScanner.next()
					if (line.startsWith("Apache Maven ")) {
						versionLine = line
						logger.info("Maven Version: '{}'", versionLine)
					} else if (line.indexOf(':') != -1) {
						logger.info("Maven Property '{}' : '{}'", line.substring(0, line.indexOf(':')).trim(),
							line.substring(line.indexOf(':') + 1).trim())
					}
				}
				return parseVersionIformation(versionLine)
			}

			def MavenVersionCheck parseVersionIformation(String versionLine) {
				if (versionLine == null) {
					return MavenVersionCheck.unknown_version
				}
				var lineWords = versionLine.split(" ")
				if (lineWords.length < 3) {
					return MavenVersionCheck.unknown_version
				} else {
					if (!lineWords.get(2).matches("[0-9]\\.*[0-9]\\.?[0-9]")) {
						return MavenVersionCheck.unknown_version
					} else {
						val versionIds = lineWords.get(2).split("\\.")

						if (Integer.parseInt(versionIds.get(0)) > MavenExecutor.MAVEN_MIMIMUM_MAJOR_VERSION) {
							return MavenVersionCheck.ok
						}
						if (Integer.parseInt(versionIds.get(0)) == MavenExecutor.MAVEN_MIMIMUM_MAJOR_VERSION &&
							Integer.parseInt(versionIds.get(1)) >= MavenExecutor.MAVEN_MIMIMUM_MINOR_VERSION) {
							return MavenVersionCheck.ok
						}
					}
				}
			}

			def boolean checkJdkIsUsed() {
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

