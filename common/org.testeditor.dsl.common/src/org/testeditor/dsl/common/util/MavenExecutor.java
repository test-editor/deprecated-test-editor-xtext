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
import java.util.Collection;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.TimeUnit;

import javax.inject.Inject;

import org.apache.maven.cli.MavenCli;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.m2e.core.internal.Bundles;
import org.eclipse.m2e.core.internal.MavenPluginActivator;
import org.osgi.framework.Bundle;
import org.osgi.framework.FrameworkUtil;
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

	@Inject
	private OSUtil osUtil;

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
			OutputStream outputStream, boolean useJvmClasspath) throws IOException {
		List<String> command = createMavenExecCommand(parameters, pathToPom, testParam, useJvmClasspath);
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

	private List<String> createMavenExecCommand(String parameters, String pathToPom, String testParam,
			boolean useJvmClasspath) {
		List<String> command = new ArrayList<String>();
		if (useJvmClasspath && System.getenv("MAVEN_HOME") != null) {
			command.addAll(getExecuteMavenScriptCommand(parameters, testParam));
		} else {
			command.addAll(getExecuteEmbeddedMavenCommand(parameters, pathToPom, testParam, useJvmClasspath));
		}
		if (Boolean.getBoolean("te.workOffline")) {
			command.add("-o");
		}
		return command;
	}

	protected Collection<? extends String> getExecuteEmbeddedMavenCommand(String parameters, String pathToPom,
			String testParam, boolean useJvmClasspath) {
		List<String> command = new ArrayList<String>();
		String jvm = System.getProperty("java.home") + File.separator + "bin" + File.separator + "java";
		command.add(jvm);
		command.add("-cp");
		String mvnClasspath = getClassPath(useJvmClasspath);
		logger.info("Using Classpath {} for maven", mvnClasspath);
		command.add(mvnClasspath);
		Properties props = System.getProperties();
		for (String key : props.stringPropertyNames()) {
			if (key.startsWith("http.") | key.startsWith("TE.") | key.startsWith("https.")) {
				command.add("-D" + key + "=" + props.getProperty(key));
			}
		}
		command.add(this.getClass().getName());
		command.add(parameters);
		command.add(pathToPom);
		command.add(testParam);
		return command;
	}

	protected List<String> getExecuteMavenScriptCommand(String parameters, String testParam) {
		List<String> command = new ArrayList<String>();
		if (osUtil.isWindows()) {
			command.add(System.getenv("MAVEN_HOME") + "\\bin\\mvn.bat");
		} else {
			command.add(System.getenv("MAVEN_HOME") + "/bin/mvn");
		}
		command.add(parameters);
		command.add("-D" + testParam);
		return command;
	}

	/**
	 * Builds the classpath to maven embedder libs, this class and the
	 * dependencies.
	 * 
	 * @param useJvmClasspath
	 * 
	 * @return String with the classpath
	 */
	private String getClassPath(boolean useJvmClasspath) {
		if (useJvmClasspath) {
			return System.getProperty("java.class.path");
		}
		List<String> cp = new ArrayList<String>();
		cp.addAll(Bundles.getClasspathEntries(Bundles
				.findDependencyBundle(MavenPluginActivator.getDefault().getBundle(), "org.eclipse.m2e.maven.runtime")));
		cp.addAll(Bundles.getClasspathEntries(Bundles
				.findDependencyBundle(MavenPluginActivator.getDefault().getBundle(), "org.eclipse.equinox.common")));
		Bundle bundle = FrameworkUtil.getBundle(this.getClass());
		cp.addAll(Bundles.getClasspathEntries(bundle));
		for (String sname : new String[] { "org.slf4j.api", "org.eclipse.m2e.maven.runtime.slf4j.simple",
				"javax.inject" }) {
			Bundle dependency = Bundles.findDependencyBundle(Bundles.findDependencyBundle(
					MavenPluginActivator.getDefault().getBundle(), "org.eclipse.m2e.maven.runtime"), sname);
			if (dependency != null) {
				cp.addAll(Bundles.getClasspathEntries(dependency));
			}
		}
		StringBuffer sb = new StringBuffer();
		sb.append("\"");
		for (String string : cp) {
			sb.append(string).append(File.pathSeparator);
		}
		sb.append("\"");
		return sb.toString();
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
