package org.testeditor.xmllibrary.dsl.importer.ui.wizard

import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.jface.wizard.Wizard
import org.eclipse.ui.IImportWizard
import org.eclipse.ui.IWorkbench

/** 
 * Wizard generated using new-plugin wizard and converted to Xtend.
 */
class ImportWizard extends Wizard implements IImportWizard {
	
	ImportWizardPage mainPage

	override boolean performFinish() {
		val file = mainPage.createNewFile
		return (file !== null)
	}

	override void init(IWorkbench workbench, IStructuredSelection selection) {
		windowTitle = "XMLLibrary Import Wizard"
		needsProgressMonitor = true
		mainPage = new ImportWizardPage("Import AllActionGroups / TechnicalBindings File", selection)
	}

	override void addPages() {
		super.addPages()
		addPage(mainPage)
	}

}
