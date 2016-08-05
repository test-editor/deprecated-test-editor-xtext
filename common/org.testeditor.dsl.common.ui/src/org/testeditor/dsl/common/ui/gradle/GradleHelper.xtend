package org.testeditor.dsl.common.ui.gradle

import java.io.File
import java.util.function.Consumer
import org.eclipse.core.resources.IProject
import org.gradle.tooling.BuildLauncher
import org.gradle.tooling.GradleConnector
import org.gradle.tooling.ResultHandler
import org.gradle.tooling.events.OperationType
import org.slf4j.LoggerFactory
import java.io.OutputStream

/**
 * Helper class for running Gradle builds.
 */
class GradleHelper {

	private static val logger = LoggerFactory.getLogger(GradleHelper)
	
	private OutputStream out;

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
		val connector = GradleConnector.newConnector.forProjectDirectory(projectFolder).connect
		try {
			val build = connector.newBuild
			build.addProgressListener([ event |
				logger.debug("Gradle build event='{}'", event.displayName)
				out.write((event.displayName+"\n").bytes)
				out.flush
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
	
	def setOutput(OutputStream stream) {
		out = stream
	}

}
