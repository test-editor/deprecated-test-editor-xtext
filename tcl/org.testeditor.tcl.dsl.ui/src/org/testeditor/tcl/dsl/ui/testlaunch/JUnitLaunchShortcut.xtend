package org.testeditor.tcl.dsl.ui.testlaunch

import javax.inject.Inject
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.Platform
import org.eclipse.jface.viewers.ISelection
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.jface.viewers.StructuredSelection
import org.eclipse.ui.IEditorPart

/**
 * Launch shortcut that is specifically crafted to execute Tests based on TCL files
 */
class JUnitLaunchShortcut extends org.eclipse.jdt.junit.launcher.JUnitLaunchShortcut {

	val EXTENSION_POINT_LAUNCHER_ID = "org.testeditor.tcl.dsl.ui.tcl_launcher"
	val EXTENSION_POINT_CLASS_ATTRIBUTE = "class"

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
			val je = res.javaElementForResource
			if (je != null) {
				val selection = new StructuredSelection(je)
				val successfulLauncher = extensions.findFirst [ // should be a firstThat
					launch(selection, je.javaProject.getAdapter(IProject), je.elementId, mode)
				]
				if (successfulLauncher == null) {
					super.launch(selection, mode)
				}
			}
		}
	}

	private def Launcher[] getExtensions() {
		val conf = Platform.getExtensionRegistry().getConfigurationElementsFor(EXTENSION_POINT_LAUNCHER_ID);
		return conf.map[createExecutableExtension(EXTENSION_POINT_CLASS_ATTRIBUTE) as Launcher]
	}

}
