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
package org.testeditor.dsl.common.util

import java.io.File

class MavenCommand {

	def public void execute(File pomDirectory, String... commands) {
		val processBuilder = new ProcessBuilder()
		val commandString = newArrayList()
		commandString.add(System.getenv("MAVEN_HOME") + "/bin/mvn")
		commandString.addAll(commands)
		processBuilder.command(commandString)
		processBuilder.directory(pomDirectory)
		processBuilder.inheritIO
		val process = processBuilder.start
		process.waitFor
	}
}
