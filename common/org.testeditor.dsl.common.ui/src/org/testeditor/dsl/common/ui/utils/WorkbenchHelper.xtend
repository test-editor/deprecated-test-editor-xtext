package org.testeditor.dsl.common.ui.utils

import org.eclipse.ui.PlatformUI
import org.eclipse.ui.IWorkbench
import org.eclipse.ui.IDecoratorManager

class WorkbenchHelper {
	
	def IWorkbench getWorkbench() {
		return PlatformUI.workbench
	}
	
	def IDecoratorManager getDecoratorManager() {
		return workbench.decoratorManager
	}
	
	
}