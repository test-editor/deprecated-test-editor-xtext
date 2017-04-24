package org.testeditor.tcl.dsl.ui.wizard

import javax.inject.Inject
import org.testeditor.dsl.common.ui.wizards.NewFileWizard
import org.testeditor.dsl.common.ui.wizards.NewFileWizardPage

class NewConfigFileWizard extends NewFileWizard {

	@Inject NewFileWizardPage page

	override String getContainerName() {
		page.containerName
	}

	override String getFileName() {
		page.fileName
	}

	override void addPages() {
		page.init(selection, "New Config File", "This wizard creates a new file with *.config extension.", "config")
		addPage(page)
	}

	override String contentString(String thePackage, String fileName) {
		return '''
			config «fileName.replace(".config", "").toFirstUpper»
			
			Setup:
			
			Cleanup:
		'''
	}
	
}