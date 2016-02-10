package org.testeditor.rcp4.product

import org.eclipse.core.runtime.Platform
import org.eclipse.core.runtime.dynamichelpers.IExtensionChangeHandler
import org.eclipse.swt.graphics.Point
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.application.IActionBarConfigurer
import org.eclipse.ui.application.IWorkbenchWindowConfigurer
import org.eclipse.ui.application.WorkbenchWindowAdvisor
import org.eclipse.ui.internal.dialogs.WorkbenchWizardElement
import org.eclipse.ui.internal.ide.EditorAreaDropAdapter
import org.eclipse.ui.wizards.IWizardCategory
import org.eclipse.ui.wizards.IWizardDescriptor

class ApplicationWorkbenchWindowAdvisor extends WorkbenchWindowAdvisor {
	new(IWorkbenchWindowConfigurer configurer) {
		super(configurer)
	}

	override createActionBarAdvisor(IActionBarConfigurer configurer) {
		new ApplicationActionBarAdvisor(configurer)
	}

	override preWindowOpen() {
		windowConfigurer => [
			initialSize = new Point(800, 600)
			showCoolBar = false
			showStatusLine = false
			title = "Testeditor" // $NON-NLS-1$
			// configuring the drop listener is necessary, since this is done during ide startup but not during rcp startup
			// if not configured, drag and drop of text within and to editors is not functional !
			configureEditorAreaDropListener(new EditorAreaDropAdapter(PlatformUI.workbench.activeWorkbenchWindow))
		]
	}

	override postWindowOpen() {
		removeUnwantedWizards
	}

	def void removeUnwantedWizards() {
		val wizardRegistry = PlatformUI.workbench.newWizardRegistry
		val categories = PlatformUI.workbench.newWizardRegistry.rootCategory.categories
		categories.allWizards.filter[!(category.id.matches("org.eclipse.ui.Basic") || id.matches("org.testeditor.*"))].
			forEach [
				val wizardElement = Platform.adapterManager.getAdapter(it, WorkbenchWizardElement)
				(wizardRegistry as IExtensionChangeHandler).removeExtension(
					wizardElement.configurationElement.declaringExtension, #[wizardElement])
			]
	}

	def IWizardDescriptor[] getAllWizards(IWizardCategory[] wizardCategories) {
		wizardCategories.map[wizards + categories.allWizards].flatten
	}
}
