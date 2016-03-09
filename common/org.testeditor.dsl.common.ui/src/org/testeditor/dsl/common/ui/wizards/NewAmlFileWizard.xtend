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

class NewAmlFileWizard extends NewFileWizard {

	@Inject NewFileWizardPage amlPage

	override String getContainerName() {
		amlPage.containerName
	}

	override String getFileName() {
		amlPage.fileName
	}

	override void addPages() {
		amlPage.init(selection, "New Aml File", "This wizard creates a new file with *.aml extension.", "aml")
		addPage(amlPage)
	}

	override InputStream contentStream(String thePackage, String fileName) {
		return new StringInputStream('''
			package «thePackage ?: "com.example"»
			
			component «fileName.replace(".aml","").toFirstUpper» is Application {				
			}
		''')
	}

}
