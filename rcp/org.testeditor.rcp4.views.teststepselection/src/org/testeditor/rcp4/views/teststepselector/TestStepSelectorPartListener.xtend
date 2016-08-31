package org.testeditor.rcp4.views.teststepselector

import org.eclipse.ui.IPartListener
import org.eclipse.ui.IWorkbenchPart
import org.eclipse.ui.texteditor.ITextEditor
import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor

@FinalFieldsConstructor
class TestStepSelectorPartListener implements IPartListener {

	val TestStepSelector testStepSelector

	override partActivated(IWorkbenchPart part) {
		if (part instanceof ITextEditor) {
			testStepSelector.refreshView(null)
		}
	}

	override partBroughtToTop(IWorkbenchPart part) {
	}

	override partClosed(IWorkbenchPart part) {
	}

	override partDeactivated(IWorkbenchPart part) {
	}

	override partOpened(IWorkbenchPart part) {
	}

}
