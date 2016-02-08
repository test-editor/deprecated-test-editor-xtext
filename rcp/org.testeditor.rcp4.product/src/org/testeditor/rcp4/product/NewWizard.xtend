package org.testeditor.rcp4.product

import org.eclipse.core.resources.IProject
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jdt.junit.JUnitCore
import org.eclipse.jdt.ui.PreferenceConstants
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard
import org.eclipse.xtext.ui.XtextProjectHelper

/** wizard to create a new test project 
 *  - add java nature
 *  - add xtext nature
 *  - add junit to the classpath 
 */
class NewWizard extends BasicNewProjectResourceWizard {

	def addNature(IProject newProject, String nature) {
		if (!newProject.hasNature(nature)) {
			val description = newProject.getDescription
			description.setNatureIds(description.getNatureIds + #[nature])
			newProject.setDescription(description, null)
		}
	}

	override performFinish() {
		val result = super.performFinish()

		val folder = newProject.getFolder("src-gen");
		folder.create(false, true, null)

		newProject.addNature(JavaCore.NATURE_ID)
		JavaCore.create(newProject) =>
			[
				val rawPath = PreferenceConstants.defaultJRELibrary +
					#[JavaCore.newSourceEntry(folder.fullPath),
						JavaCore.newContainerEntry(JUnitCore.JUNIT4_CONTAINER_PATH)]
				setRawClasspath(rawPath, null)
			]
		newProject.addNature(XtextProjectHelper.NATURE_ID)
		return result
	}
}
