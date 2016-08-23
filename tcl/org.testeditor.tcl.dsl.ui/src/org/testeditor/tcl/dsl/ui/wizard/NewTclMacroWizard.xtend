/*******************************************************************************
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 *******************************************************************************/
package org.testeditor.tcl.dsl.ui.wizard

import javax.inject.Inject
import org.testeditor.dsl.common.ui.wizards.NewFileWizard
import org.testeditor.dsl.common.ui.wizards.NewFileWizardPage

class NewTclMacroWizard extends NewFileWizard {

	@Inject NewFileWizardPage tmlPage

	override String getContainerName() {
		tmlPage.containerName
	}

	override String getFileName() {
		tmlPage.fileName
	}

	override void addPages() {
		tmlPage.init(selection, "New Test Macro", "This wizard creates a new macro with *.tml extension.", "tml")
		addPage(tmlPage)
	}

	override String contentString(String thePackage, String fileName) {
		return '''
			package «thePackage ?: "com.example"»
			
			# «fileName.replace(".tml","").toFirstUpper»

			## «fileName.replace(".tml","").toFirstUpper»
			template = "«fileName.replace(".tml","").toFirstUpper»"
		'''
	}
}
