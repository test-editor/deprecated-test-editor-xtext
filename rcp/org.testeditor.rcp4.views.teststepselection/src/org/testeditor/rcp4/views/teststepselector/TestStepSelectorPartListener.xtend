package org.testeditor.rcp4.views.teststepselector

import org.eclipse.ui.IPartListener
import org.eclipse.ui.IWorkbenchPart
import org.eclipse.ui.texteditor.ITextEditor

class TestStepSelectorPartListener  implements IPartListener {

	val TestStepSelector testStepSelector 
	
	new(TestStepSelector testStepSelector) {
		this.testStepSelector = testStepSelector
	}
	
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