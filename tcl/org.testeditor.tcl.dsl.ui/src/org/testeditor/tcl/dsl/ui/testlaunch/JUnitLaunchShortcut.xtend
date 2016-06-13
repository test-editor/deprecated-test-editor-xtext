package org.testeditor.tcl.dsl.ui.testlaunch

import java.util.HashMap
import java.util.Set
import javax.inject.Inject
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.IConfigurationElement
import org.eclipse.core.runtime.Platform
import org.eclipse.jface.viewers.ISelection
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.jface.viewers.StructuredSelection
import org.eclipse.ui.IEditorPart
import org.slf4j.LoggerFactory

/**
 * Launch shortcut that is specifically crafted to execute Tests based on TCL files
 */
class JUnitLaunchShortcut extends org.eclipse.jdt.junit.launcher.JUnitLaunchShortcut {

	static val logger = LoggerFactory.getLogger(JUnitLaunchShortcut)

	public val EXTENSION_POINT_LAUNCHER_ID = "org.testeditor.tcl.dsl.ui.tcl_launcher"
	public val EXTENSION_POINT_CLASS_ATTRIBUTE = "class"

	val launcherMap = new HashMap<Launcher, IConfigurationElement>

	@Inject extension LaunchShortcutUtil

	override void launch(ISelection selection, String mode) {
		if (selection instanceof IStructuredSelection) {
			launch(selection.firstElement as IResource, mode, false)
		}
	}

	override void launch(IEditorPart editor, String mode) {
		launch(editor.editorInput.getAdapter(IResource), mode, false)
	}

	protected def launch(IResource res, String mode, boolean parameterize) {
		if (!res.isValidForTestrun) {
			logger.warn("resource='{}' seems to be invalid for test run (e.g. has error markers)", res.fullPath)
		}
		val selection = new StructuredSelection(res)
		val qualifiedName = res.getQualifiedNameForTestInTcl
		if (qualifiedName == null) {
			logger.error("resource='{}' seems not to export any test.", res.fullPath)
		} else {
			val successfulLauncher = registeredLaunchers.findFirst [ registeredLauncher | // firstThat would be more fitting in this case
				val result = registeredLauncher.launch(selection, res.project, qualifiedName.toString, mode,
					parameterize)
				if (result) {
					logger.debug("executed registeredLauncher='{}' for tcl test launch.",
						launcherMap.get(registeredLauncher).toLoggingString)
				} else {
					logger.warn("execution fo registeredLauncher='{}' failed",
						launcherMap.get(registeredLauncher).toLoggingString)
				}
				return result
			]
			if (successfulLauncher == null) { // fallback
				super.launch(selection, mode)
				logger.debug("executed junit launcher for tcl test launch")
			}
		}
	}

	private def String toLoggingString(IConfigurationElement configurationElement) {
		"contributed by " + configurationElement.contributor.name
	}

	private def Set<Launcher> getRegisteredLaunchers() {
		if (launcherMap.empty) {
			val conf = Platform.getExtensionRegistry().getConfigurationElementsFor(EXTENSION_POINT_LAUNCHER_ID);
			conf.forEach [
				val launcher = createExecutableExtension(EXTENSION_POINT_CLASS_ATTRIBUTE) as Launcher
				launcherMap.put(launcher, it)
			]
		}
		return launcherMap.keySet
	}

}
