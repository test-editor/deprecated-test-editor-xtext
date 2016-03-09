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
package org.testeditor.dsl.common.ui.wizards

import java.io.InputStream
import javax.inject.Inject
import org.eclipse.xtext.util.StringInputStream

class NewTslFileWizard extends NewFileWizard {

	@Inject NewFileWizardPage tslPage

	override String getContainerName() {
		tslPage.containerName
	}

	override String getFileName() {
		tslPage.fileName
	}

	override void addPages() {
		tslPage.init(selection, "New Tsl File", "This wizard creates a new file with *.tsl extension.", "tsl")
		addPage(tslPage)
	}

	override InputStream contentStream(String thePackage, String fileName) {
		return new StringInputStream('''
			package «thePackage ?: "com.example"»
			
			# «fileName.replace(".tsl","").toFirstUpper» 
			
			Short description
			=================
			
			Your description of the test case goes here.
			
			Test steps
			==========
			
			The following steps are referenced and detailed with the Tcl
			
			* Test one
			* Test two
		''')
	}
}
