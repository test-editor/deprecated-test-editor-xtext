package org.testeditor.dsl.common.ui.utils

import org.eclipse.e4.core.di.annotations.Creatable
import org.eclipse.jface.dialogs.MessageDialog
import org.eclipse.ui.IDecoratorManager
import org.eclipse.ui.IWorkbench
import org.eclipse.ui.IWorkbenchWindow
import org.eclipse.ui.PlatformUI

@Creatable
class WorkbenchHelper {
	
	def IWorkbench getWorkbench() {
		return PlatformUI.workbench
	}
	
	def IDecoratorManager getDecoratorManager() {
		return workbench.decoratorManager
	}
	
	def IWorkbenchWindow getActiveWorkbenchWindow() {
		return workbench.activeWorkbenchWindow
	}

	def boolean answerYesNoErrorMessageDialog(String title, String message) {
		new MessageDialog(activeWorkbenchWindow.shell, title, null, message,
			MessageDialog.ERROR, #{"Yes", "No"}, 0).open > 0
	}
	
}