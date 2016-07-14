package org.testeditor.tcl.dsl.ui.testlaunch

import org.testeditor.tcl.dsl.ui.testlaunch.JUnitLaunchShortcut
import org.eclipse.jface.viewers.ISelection
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.core.resources.IResource
import org.eclipse.ui.IEditorPart
import org.eclipse.jface.viewers.StructuredSelection

class JUnitLaunchShortcutParameterized extends JUnitLaunchShortcut {
	override void launch(ISelection selection, String mode) {
		if (selection instanceof IStructuredSelection) {
			launch(selection, mode, true)
		}
	}

	override void launch(IEditorPart editor, String mode) {
		launch(new StructuredSelection(editor.editorInput.getAdapter(IResource)), mode, true)
	}

}