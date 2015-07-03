package org.testeditor.aml.dsl.ui.wizard

import org.testeditor.aml.dsl.ui.wizard.AmlProjectCreator
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.CoreException

/**
 * Keeps the project clean for now.
 */
class CustomizedAmlProjectCreator extends AmlProjectCreator {
	
	override protected getRequiredBundles() {
		return #[]
	}	
	
	override protected getImportedPackages() {
		return #[]
	}
	
	override protected enhanceProject(IProject project, IProgressMonitor monitor) throws CoreException {
		// do nothing
	}
	
}