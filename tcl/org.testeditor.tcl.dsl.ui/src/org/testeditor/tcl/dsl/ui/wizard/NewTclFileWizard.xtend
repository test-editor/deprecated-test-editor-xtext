/*******************************************************************************
 * Copyright (c) 2012 - 2018 Signal Iduna Corporation and others.
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

class NewTclFileWizard extends NewFileWizard {

	@Inject NewFileWizardPage tclPage

	override String getContainerName() {
		tclPage.containerName
	}

	override String getFileName() {
		tclPage.fileName
	}

	override void addPages() {
		tclPage.init(selection, "New Tcl File", "This wizard creates a new file with *.tcl extension.", "tcl")
		addPage(tclPage)
	}

	override String contentString(String thePackage, String fileName) {
		return '''
			# «fileName.replace('.tcl','').toFirstUpper»
		'''
	}
}
