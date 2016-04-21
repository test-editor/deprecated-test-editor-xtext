package org.testeditor.tcl.dsl.ui.testlaunch

import org.testeditor.tcl.dsl.ui.testlaunch.JUnitLaunchShortcut
import org.eclipse.jface.viewers.ISelection
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.core.resources.IResource
import org.eclipse.ui.IEditorPart

class JUnitLaunchShortcutParameterized extends JUnitLaunchShortcut {
	override void launch(ISelection selection, String mode) {
		if (selection instanceof IStructuredSelection) {
			launch(selection.firstElement as IResource, mode, true)
		}
	}

	override void launch(IEditorPart editor, String mode) {
		launch(editor.editorInput.getAdapter(IResource), mode, true)
	}

}