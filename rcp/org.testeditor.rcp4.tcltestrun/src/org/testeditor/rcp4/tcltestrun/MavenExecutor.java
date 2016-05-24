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
package org.testeditor.rcp4.tcltestrun;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.apache.maven.cli.MavenCli;
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
public class MavenExecutor {

	private static Logger logger = LoggerFactory.getLogger(MavenExecutor.class);

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
		int result = cli.doMain(parameters.split(" "), pathToPom, System.out, System.err);
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
	 * @return int with exit code
	 * @throws IOException
	 *             on failure
	 */
	public int executeInNewJvm(String parameters, String pathToPom, String testParam) throws IOException {
		int result = 1; // unspecified error code != 0
		String jvm = System.getProperty("java.home") + File.separator + "bin" + File.separator + "java";
		List<String> command = new ArrayList<String>();
		command.add(jvm);
		command.add("-cp");
		command.add(getClassPath());
		Properties props = System.getProperties();
		for (String key : props.stringPropertyNames()) {
			if (key.startsWith("te.")) {
				command.add("-D" + key.substring(key.indexOf(".") + 1) + "=" + props.getProperty(key));
			}
		}
		command.add(this.getClass().getName());
		command.add(parameters);
		command.add(pathToPom);
		command.add(testParam);
		String mavenSettings = System.getProperty("TE.MAVENSETTINGSPATH");
		if (mavenSettings != null) {
			if (mavenSettings.length() > 0) {
				command.add("-s");
				command.add(mavenSettings);
			}
		}
		ProcessBuilder processBuilder = new ProcessBuilder();
		processBuilder.inheritIO();
		processBuilder.directory(new File(pathToPom));
		processBuilder.redirectErrorStream(true);
		logger.info("Execute maven in new jvm with: {}", command);
		processBuilder.command(command);
		Process process = processBuilder.start();
		try {
			result = process.waitFor();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		return result;
	}

	/**
	 * Builds the classpath to maven embedder libs, this class and the
	 * dependencies.
	 * 
	 * @return String with the classpath
	 */
	private String getClassPath() {
		List<String> cp = new ArrayList<String>();
		cp.addAll(Bundles.getClasspathEntries(Bundles
				.findDependencyBundle(MavenPluginActivator.getDefault().getBundle(), "org.eclipse.m2e.maven.runtime")));
		Bundle bundle = FrameworkUtil.getBundle(this.getClass());
		cp.addAll(Bundles.getClasspathEntries(bundle));
		Bundle[] bundles = bundle.getBundleContext().getBundles();
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
