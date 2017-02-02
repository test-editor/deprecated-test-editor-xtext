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
package org.testeditor.dsl.common.util;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

import javax.inject.Inject;

import org.apache.commons.lang3.SystemUtils;
import org.apache.maven.cli.MavenCli;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Executes a maven build in a new jvm using the embedded maven. The maven
 * embedder api allows a maven build without a m2 variable configuration.
 *
 */
@SuppressWarnings("restriction")
public class MavenExecutor {

	private static Logger logger = LoggerFactory.getLogger(MavenExecutor.class);
	public static int MAVEN_MIMIMUM_MAJOR_VERSION = 3;
	public static int MAVEN_MIMIMUM_MINOR_VERSION = 2;
	public static String TE_MAVEN_HOME = "MAVEN_HOME";

	public enum MavenVersionCheck {
		wrong_version,
		unknown_version,
		ok,
		no_maven
	}
	
	/**
	 * Executes the maven build using maven embedder. It allways starts with a
	 * clean operation to delete old test results.
	 * 
	 * @param parameters
	 *            for maven (separated by spaces, e.g. "clean integration-test"
	 *            to execute the given goals)
	 * @param pathToPom
	 *            path to the folder where the pom.xml is located.
	 * @return int with exit code
	 * 
	 */
	public int execute(String parameters, String pathToPom) {
		System.setProperty("maven.multiModuleProjectDirectory", pathToPom);
		MavenCli cli = new MavenCli();
		List<String> params = new ArrayList<String>();
		params.addAll(Arrays.asList(parameters.split(" ")));
		String mavenSettings = System.getProperty("TE.MAVENSETTINGSPATH");
		if (mavenSettings != null && mavenSettings.length() > 0) {
			params.add("-s");
			params.add(mavenSettings);
		}
		int result = cli.doMain(params.toArray(new String[] {}), pathToPom, System.out, System.err);
		return result;
	}

	/**
	 * Executes a maven build in a new jvm. The executable of the current jvm is
	 * used to create a new jvm.
	 * 
	 * @param parameters
	 *            for maven (separated by spaces, e.g. "clean integration-test"
	 *            to execute the given goals)
	 * @param pathtoPom
	 *            path to the folder where the pom.xml is located.
	 * @param testParam
	 *            pvm parameter to identify the test case to be executed.
	 * @param monitor
	 *            Progress monitor to handle cancel events.
	 * @return the result interpreted as {@link IStatus}.
	 * @throws IOException
	 *             on failure
	 */
	public int executeInNewJvm(String parameters, String pathToPom, String testParam, IProgressMonitor monitor,
			OutputStream outputStream) throws IOException {
		List<String> command = createMavenExecCommand(parameters, pathToPom, testParam);
		ProcessBuilder processBuilder = new ProcessBuilder();
		processBuilder.directory(new File(pathToPom));
		logger.info("Executing maven in new jvm with command={}", command);
		processBuilder.command(command);
		Process process = processBuilder.start();
		PrintStream out = new PrintStream(outputStream);
		OutputStreamCopyUtil outputCopyThread = new OutputStreamCopyUtil(process.getInputStream(), out);
		OutputStreamCopyUtil errorCopyThread = new OutputStreamCopyUtil(process.getErrorStream(), out);
		outputCopyThread.start();
		errorCopyThread.start();
		try {
			while (!process.waitFor(100, TimeUnit.MILLISECONDS)) {
				if (monitor.isCanceled()) {
					process.destroy();
					out.println("Operation cancelled.");
					return IStatus.CANCEL;
				}
			}
			return process.exitValue() == 0 ? IStatus.OK : IStatus.CANCEL;
		} catch (InterruptedException e) {
			logger.error("Caught exception.", e);
			return IStatus.ERROR;
		}
	}

	private List<String> createMavenExecCommand(String parameters, String pathToPom, String testParam) {
		List<String> command = new ArrayList<String>();
		command.addAll(getExecuteMavenScriptCommand(parameters, testParam,SystemUtils.IS_OS_WINDOWS));
		if (Boolean.getBoolean("te.workOffline")) {
			command.add("-o");
		}
		return command;
	}


	protected List<String> getExecuteMavenScriptCommand(String parameters, String testParam, boolean isOsWindows) {
		List<String> command = new ArrayList<String>();
		command.add(getPathToMavenExecutable(isOsWindows));
		command.add(parameters);
		command.add("-D" + testParam);
		return command;
	}

	public static String getPathToMavenExecutable(boolean isOsWindows) {
		String mavenHome = System.getenv(TE_MAVEN_HOME);
		if(mavenHome == null){
			return "";
		}
		if (isOsWindows) {
			return mavenHome + "\\bin\\mvn.bat";
		} else {
			return mavenHome + "/bin/mvn";
		}
	}
	/**
	 * Entry point of the new jvm used for the maven build.
	 * 
	 * @param args
	 *            the maven parameter.
	 */
	public static void main(String[] args) {
		logger.info("Proxy host: {}", System.getProperty("http.proxyHost"));
		if (args.length > 2) {
			if (args[2].contains("=")) {
				logger.info("Running maven build with settings='{}'", args[2]);
				String[] split = args[2].split("=");
				System.setProperty(split[0], split[1]);
			} else {
				logger.warn("Running maven build IGNORING MISSPELLED settings='{}' (missing infix '=')", args[2]);
			}
		} else {
			logger.info("Running maven build without settings");
		}
		int result = new MavenExecutor().execute(args[0], args[1]);
		if (result != 0) {
			System.exit(result);
		}
	}

}
