package org.testeditor.tcl.dsl.ui.testlaunch

import javax.inject.Inject
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.Platform
import org.eclipse.jface.viewers.ISelection
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.jface.viewers.StructuredSelection
import org.eclipse.ui.IEditorPart
import org.slf4j.LoggerFactory
import org.eclipse.core.runtime.IConfigurationElement
import java.util.HashMap
import java.util.Map
import java.util.Set

/**
 * Launch shortcut that is specifically crafted to execute Tests based on TCL files
 */
class JUnitLaunchShortcut extends org.eclipse.jdt.junit.launcher.JUnitLaunchShortcut {

	static val logger = LoggerFactory.getLogger(JUnitLaunchShortcut)

	val EXTENSION_POINT_LAUNCHER_ID = "org.testeditor.tcl.dsl.ui.tcl_launcher"
	val EXTENSION_POINT_CLASS_ATTRIBUTE = "class"

	val launcherMap = new HashMap<Launcher, IConfigurationElement>

	@Inject
	extension LaunchShortcutUtil launchUtil

	override void launch(ISelection selection, String mode) {
		if (selection instanceof IStructuredSelection) {
			launch(selection.firstElement as IResource, mode)
		}
	}

	override void launch(IEditorPart editor, String mode) {
		launch(editor.editorInput.getAdapter(IResource), mode)
	}

	private def launch(IResource res, String mode) {
		if (res.isValidForTestrun) {
			val javaElement = res.javaElementForResource
			val selection = new StructuredSelection(javaElement)
			val successfulLauncher = registeredLaunchers.findFirst [ // should be a firstThat
				val result = launch(selection, javaElement.javaProject.getAdapter(IProject), javaElement.elementId,
					mode)
				if (result) {
					logger.debug("executed launcher registered for tcl test launch: " + launcherMap.get(it).logStringFor)
				} else {
					logger.warn("registered tcl test launcher failed: " + launcherMap.get(it).logStringFor)
				}
				return result
			]
			if (successfulLauncher == null) {
				super.launch(selection, mode)
				logger.debug("executed junit launcher for tcl test launch")
			}
		}
	}
	
	private def String logStringFor(IConfigurationElement configurationElement){
		"contributed by "+configurationElement.contributor.name
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
