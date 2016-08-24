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

import java.io.ByteArrayOutputStream
import java.io.File
import java.io.PrintStream

class GradleCommand {
	
	def String execute(File gradleBuildDirectory, String... commands) {
		val processBuilder = new ProcessBuilder()
		val commandString = newArrayList()
		commandString.add("gradlew")
		commandString.addAll(commands)
		processBuilder.command(commandString)
		processBuilder.directory(gradleBuildDirectory)
		processBuilder.inheritIO
		val process = processBuilder.start
		val out = new ByteArrayOutputStream()
		new OutputStreamCopyUtil(process.getInputStream(), new PrintStream(out));
		process.waitFor
		return out.toString
	}
	
}