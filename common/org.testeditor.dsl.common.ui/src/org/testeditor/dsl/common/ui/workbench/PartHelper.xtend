package org.testeditor.dsl.common.ui.workbench

import org.eclipse.ui.PlatformUI

class PartHelper {

	def void showView(String viewId) {
		PlatformUI.workbench.display.syncExec [
			val window = PlatformUI.workbench.workbenchWindows.head
			window.activePage.showView(viewId)
		]
	}

}
