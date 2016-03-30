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

import javax.inject.Inject
import org.eclipse.core.resources.FileInfoMatcherDescription
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.IResourceFilterDescription
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jdt.junit.JUnitCore
import org.eclipse.jdt.ui.PreferenceConstants
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.ui.IWorkbench
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard
import org.eclipse.xtext.ui.XtextProjectHelper
import org.testeditor.dsl.common.ui.utils.ProjectUtils

/** wizard to create a new test project 
 *  - add java nature
 *  - add xtext nature
 *  - add junit to the classpath 
 */
class NewProjectWizard extends BasicNewProjectResourceWizard {

	@Inject extension ProjectUtils

	static public val String SRC_FOLDER = 'src/main/java'

	private def void addNature(IProject newProject, String nature) {
		if (!newProject.hasNature(nature)) {
			val description = newProject.getDescription
			description.setNatureIds(description.getNatureIds + #[nature])
			newProject.setDescription(description, null)
		}
	}

	override init(IWorkbench bench, IStructuredSelection selection) {
		super.init(bench, selection)
		windowTitle = "Create test-first Testproject"
	}

	override performFinish() {
		val result = super.performFinish()

		val srcFolder = newProject.createOrGetDeepFolder(SRC_FOLDER)
		newProject.addNature(JavaCore.NATURE_ID)
		JavaCore.create(newProject) =>
			[
				val rawPath = PreferenceConstants.defaultJRELibrary +
					#[JavaCore.newSourceEntry(srcFolder.fullPath),
						JavaCore.newContainerEntry(JUnitCore.JUNIT4_CONTAINER_PATH)]
				setRawClasspath(rawPath, null)
			]
		newProject.addNature(XtextProjectHelper.NATURE_ID)
		// make sure that no target folder is included into any resource set
		newProject.createFilter(IResourceFilterDescription.EXCLUDE_ALL.bitwiseOr(IResourceFilterDescription.FOLDERS),
			new FileInfoMatcherDescription("org.eclipse.core.resources.regexFilterMatcher", "target"), // hide maven generated/copied artifacts
			IResource.BACKGROUND_REFRESH, new NullProgressMonitor)
		newProject.createFilter(IResourceFilterDescription.EXCLUDE_ALL.bitwiseOr(IResourceFilterDescription.FOLDERS),
			new FileInfoMatcherDescription("org.eclipse.core.resources.regexFilterMatcher", "build"), // hide gradle generated/copied artifacts
			IResource.BACKGROUND_REFRESH, new NullProgressMonitor)
		return result
	}

}
