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

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.SystemUtils;
import org.apache.maven.cli.MavenCli;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.annotations.VisibleForTesting;

/**
 * Executes a maven build in a new jvm using the embedded maven. The maven
 * embedder api allows a maven build without a m2 variable configuration.
 *
 */
public class MavenExecutor {

	private static Logger logger = LoggerFactory.getLogger(MavenExecutor.class);
	public static int MAVEN_MIMIMUM_MAJOR_VERSION = 3;
	public static int MAVEN_MIMIMUM_MINOR_VERSION = 2;
	public static String TE_MAVEN_HOME = "TE_MAVEN_HOME";

	public enum MavenVersionValidity {
		wrong_version, unknown_version, ok, no_maven
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

	protected String getPathToMavenHome() {
		return System.getenv(TE_MAVEN_HOME);
	}

	private List<String> createMavenExecCommand(String parameters, String pathToPom, String testParam) {
		List<String> command = new ArrayList<String>();
		command.addAll(getExecuteMavenScriptCommand(getPathToMavenHome(), parameters, testParam, SystemUtils.IS_OS_WINDOWS));
		if (Boolean.getBoolean("te.workOffline")) {
			command.add("-o");
		}
		return command;
	}

	@VisibleForTesting
	protected List<String> getExecuteMavenScriptCommand(String mavenHome, String parameters, String testParam, boolean isOsWindows) {
		List<String> command = new ArrayList<String>();
		command.add(getPathToMavenExecutable(mavenHome, isOsWindows));
		command.addAll(Arrays.asList(parameters.split(" ")));
		if (testParam != null && testParam.length() > 0) {
			command.add("-D" + testParam);
		}
		command.add("-V");
		return command;
	}
	
	private String getPathToMavenExecutable(String mavenHome, boolean isOsWindows) {
		if (mavenHome == null) {
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

	public MavenVersionValidity getMavenVersionValidity() throws IOException {
		if (getPathToMavenHome() == null) {
			logger.info("No maven found. Environment variable {}='{}' not set correctly!", TE_MAVEN_HOME, getPathToMavenHome());
			return MavenVersionValidity.no_maven;
		}
		String[] lines = linesFromMavenVersionCall();
		if (lines == null) {
			logger.info("Maven execution to get the version produced no output!");
			return MavenVersionValidity.no_maven;
		}
		
		String versionLine = findVersionLine(lines);
		if (versionLine == null) {
			return MavenVersionValidity.unknown_version;
		}
		logger.info("Maven Version: '{}'", versionLine);
		logMavenPropertiesIn(lines);
		
		int[] version=parseVersionInformation(versionLine);
		return validateVersionInformation(version);
	}
	
	private void logMavenPropertiesIn(String[] lines) {
		for (String line : lines) {
			 if (line.indexOf(": ") >= 0) {
				String[] keyPropPair=line.split(": ", 2);
				logger.info("Maven Property '{}' : '{}'", keyPropPair[0].trim(), keyPropPair[1].trim());
			}
		}
	}
	
	private String findVersionLine(String[] lines){
		for (String line : lines) {
			if (line.startsWith("Apache Maven ")) {
				return line;
			}
		}
		return null;
	}
	
	private String[] linesFromMavenVersionCall() throws IOException {
		OutputStream versionOut = new ByteArrayOutputStream();
		int infoResult = executeInNewJvm("-version", ".", "", new NullProgressMonitor(), versionOut);
		if (infoResult != IStatus.OK) {
			logger.error("Error during determine of maven version");
			return null;
		}
		return versionOut.toString().split("(\r)?\n");
	}
	
	@VisibleForTesting
	protected int[] parseVersionInformation(String mavenVersionLine) {
		Pattern versionNumber = Pattern.compile("^([A-Za-z ]+)([0-9]+)\\.([0-9]+)(\\.([0-9]+))?.*");
		Matcher matcher = versionNumber.matcher(mavenVersionLine);
		if (!matcher.matches()) {
			return null;
		}
		int[] result=new int[2]; 
		result[0] = Integer.parseInt(matcher.group(2));
		result[1] = Integer.parseInt(matcher.group(3));
		return result;
	}
	
	@VisibleForTesting
	protected MavenVersionValidity validateVersionInformation(int[] versionNumbers) {
		if (versionNumbers==null || versionNumbers.length<2) {
			return MavenVersionValidity.unknown_version;
		}
		int major = versionNumbers[0];
		int minor = versionNumbers[1];
		if ((major > MAVEN_MIMIMUM_MAJOR_VERSION)
				|| (major == MAVEN_MIMIMUM_MAJOR_VERSION && minor >= MAVEN_MIMIMUM_MINOR_VERSION)) {
			return MavenVersionValidity.ok;
		}
		return MavenVersionValidity.wrong_version;
	}
	
}
