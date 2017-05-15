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
import java.util.function.Consumer
import org.eclipse.core.resources.IProject
import org.gradle.tooling.BuildLauncher
import org.gradle.tooling.GradleConnector
import org.gradle.tooling.ResultHandler
import org.gradle.tooling.events.OperationType
import org.slf4j.LoggerFactory

/**
 * Helper class for running Gradle builds.
 */
class GradleHelper {
	
	static val GRADLE_VERSION = "3.4.1"

	static val logger = LoggerFactory.getLogger(GradleHelper)

	def void runTasks(File projectFolder, String... tasks) {
		logger.info("Running gradle build with tasks='{}'.", tasks)
		run(projectFolder) [
			forTasks(tasks)
		]
	}

	def void run(File projectFolder, Consumer<BuildLauncher> configClosure) {
		run(projectFolder, null, configClosure)
	}

	def void run(File projectFolder, ResultHandler<? super Void> resultHandler, Consumer<BuildLauncher> configClosure) {
		val connector = GradleConnector.newConnector.useGradleVersion(GRADLE_VERSION).
			forProjectDirectory(projectFolder).connect
		try {
			val build = connector.newBuild
			build.addProgressListener([ event |
				logger.debug("Gradle build event='{}'", event.displayName)
			], OperationType.values)
			configClosure.accept(build)
			if (resultHandler !== null) {
				build.run(resultHandler)
			} else {
				build.run
			}
		} finally {
			connector.close
		}
	}

	def void runTasks(IProject project, String... tasks) {
		runTasks(project.toFile, tasks)
	}

	def void run(IProject project, Consumer<BuildLauncher> configClosure) {
		run(project.toFile, configClosure)
	}

	def void run(IProject project, ResultHandler<? super Void> resultHandler, Consumer<BuildLauncher> configClosure) {
		run(project.toFile, resultHandler, configClosure)
	}

	private def File toFile(IProject project) {
		return project.location.toFile
	}
	
}
