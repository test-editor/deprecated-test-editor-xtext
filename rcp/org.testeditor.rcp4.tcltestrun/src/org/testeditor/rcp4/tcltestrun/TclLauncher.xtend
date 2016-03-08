package org.testeditor.rcp4.tcltestrun

import java.io.File
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.jdt.junit.JUnitCore
import org.eclipse.jface.viewers.IStructuredSelection
import org.gradle.tooling.GradleConnectionException
import org.gradle.tooling.GradleConnector
import org.gradle.tooling.ProjectConnection
import org.gradle.tooling.ResultHandler
import org.gradle.tooling.events.ProgressEvent
import org.gradle.tooling.events.ProgressListener
import org.slf4j.LoggerFactory
import org.testeditor.tcl.dsl.ui.testlaunch.Launcher

class TclLauncher implements Launcher {
	static val logger = LoggerFactory.getLogger(TclLauncher)

	def void showTestResult(String path) {
		val testResult = new File(path)
		JUnitCore.importTestRunSession(testResult)
	}

	def boolean projectHasGradleBuild(String absoluteProjectPath) {
		new File(absoluteProjectPath + "/build.gradle").exists
	}

	override boolean launch(IStructuredSelection selection, IProject project, String elementId, String mode) {
		val projectPath = project.fullPath.makeAbsolute.toOSString
		val workspaceRoot = ResourcesPlugin.workspace.root
		val workspaceRootPath = workspaceRoot.rawLocation.makeAbsolute.toOSString

		if (!projectHasGradleBuild(workspaceRootPath + projectPath)) {
			return false
		}

		val testResultPath = '''«workspaceRootPath»«projectPath»/build/test-results/TEST-«elementId».xml'''
		val GradleConnector connector = GradleConnector.newConnector
		var ProjectConnection connection = null
		try {
			connection = connector.forProjectDirectory(new File(workspaceRootPath + projectPath)).connect();
			// val BuildEnvironment environment = connection.model(BuildEnvironment).get();
			connection.newBuild.addProgressListener(new ProgressListener {

				override statusChanged(ProgressEvent event) {
					logger.info(event.displayName)
				}

			}) // .forTasks("test") // does not work, see issue below
			.withArguments("test", "--tests", elementId) // https://issues.gradle.org/browse/GRADLE-2972
			// .setStandardOutput(System.out) // alternatively get a separate console output stream (see http://wiki.eclipse.org/FAQ_How_do_I_write_to_the_console_from_a_plug-in%3F)
			.run(new ResultHandler {

				override onComplete(Object obj) {
					testResultPath.showTestResult
				}

				override onFailure(GradleConnectionException exception) {
					logger.error("Failure: " + exception.toString, exception)
					testResultPath.showTestResult
				}

			})
		} finally {
			if (connection != null) {
				connection.close
			}
		}
		return true
	}

}
