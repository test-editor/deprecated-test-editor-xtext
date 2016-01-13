package org.testeditor.tcl.rcp.product

import org.eclipse.core.resources.IProject
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jdt.junit.JUnitCore
import org.eclipse.jdt.ui.PreferenceConstants
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard
import org.eclipse.xtext.ui.XtextProjectHelper

class NewWizard extends BasicNewProjectResourceWizard {
	
	def addNature(IProject newProject, String nature) {
		if (!newProject.hasNature(nature)) {
			val description = newProject.getDescription
			description.setNatureIds(description.getNatureIds + #[nature])
			newProject.setDescription(description, null)
		}
	}

	override performFinish() {
		val result=super.performFinish()

		// val junitPath = new Path("C:\\dev\\tools\\eclipse\\plugins\\org.junit_4.12.0.v201504281640\\junit.jar")
		val folder = newProject.getFolder("src-gen");
		folder.create(false,true,null)
		
		newProject.addNature(JavaCore.NATURE_ID)
		JavaCore.create(newProject)=>[
			val rawPath=PreferenceConstants.defaultJRELibrary
							+ #[// JavaCore.newLibraryEntry(junitPath, null, null,false),
								JavaCore.newSourceEntry(folder.fullPath),
								JavaCore.newContainerEntry(JUnitCore.JUNIT4_CONTAINER_PATH)]
			setRawClasspath(rawPath, null)
		]		
		newProject.addNature(XtextProjectHelper.NATURE_ID)
		return result
	}
}
