package org.testeditor.aml.dsl.ui.wizard

import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.IProgressMonitor

/**
 * Keeps the project clean for now.
 */
class CustomizedAmlProjectCreator extends AmlProjectCreator {

	override protected enhanceProject(IProject project, IProgressMonitor monitor) throws CoreException {
		// do nothing
	}

	override protected String[] getProjectNatures() {
		return super.projectNatures.filter[it != "org.eclipse.pde.PluginNature"]
	}
	
	override protected getBuilders() {
		return super.builders.filter[!it.startsWith("org.eclipse.pde")]
	}

}