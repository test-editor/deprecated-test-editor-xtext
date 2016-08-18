package org.testeditor.dsl.common.ui.workbench

import org.eclipse.core.resources.IFile
import org.eclipse.ui.IEditorPart
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.part.FileEditorInput

class PartHelper {

	def IEditorPart findEditor(IFile input) {
		val editorInput = new FileEditorInput(input)
		return PlatformUI.workbench.workbenchWindows.map[activePage.findEditor(editorInput)].head
	}

	def void showView(String viewId) {
		PlatformUI.workbench.display.syncExec [
			val window = PlatformUI.workbench.workbenchWindows.head
			window.activePage.showView(viewId)
		]
	}

}
