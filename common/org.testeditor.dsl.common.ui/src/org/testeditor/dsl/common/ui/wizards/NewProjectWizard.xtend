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
import org.eclipse.core.resources.IProject
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.jface.wizard.IWizardPage
import org.eclipse.ui.IWorkbench
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard
import org.eclipse.xtext.ui.XtextProjectHelper
import org.testeditor.dsl.common.ide.util.ProjectContentGenerator
import org.testeditor.dsl.common.ui.utils.ProgressMonitorRunner
import org.testeditor.dsl.common.ui.utils.ProjectUtils

/** wizard to create a new test project 
 *  - add java nature
 *  - add xtext nature
 *  - add build system 
 */
class NewProjectWizard extends BasicNewProjectResourceWizard {

	@Inject extension ProjectUtils

	static public val String SRC_FOLDER = 'src/main/java'

	TestProjectConfigurationWizardPage configPage

	@Inject ProjectContentGenerator projectContentGenerator

	private def void addNature(IProject newProject, String nature) {
		if (!newProject.hasNature(nature)) {
			val description = newProject.getDescription
			description.setNatureIds(description.getNatureIds + #[nature])
			newProject.setDescription(description, null)
		}
	}

	override init(IWorkbench bench, IStructuredSelection selection) {
		super.init(bench, selection)
		windowTitle = "Create test-first project"
	}

	override addPages() {
		super.addPages()
		configPage = new TestProjectConfigurationWizardPage("configPage")
		configPage.availableBuildSystems = projectContentGenerator.availableBuildSystems
		configPage.availableFixtureNames = projectContentGenerator.availableFixtureNames
		addPage(configPage)
	}

	override getNextPage(IWizardPage page) {
		return configPage
	}

	override performFinish() {
		val result = super.performFinish()

		newProject.createOrGetDeepFolder(SRC_FOLDER)
		newProject.addNature(JavaCore.NATURE_ID)
		JavaCore.create(newProject)
		newProject.addNature(XtextProjectHelper.NATURE_ID)

		if (!configPage.selectedFixtures.isEmpty) {
			new ProgressMonitorRunner().run [ monitor |
				projectContentGenerator.createProjectContent(newProject, configPage.selectedFixtures,
					configPage.buildSystemName, configPage.withDemoCode, monitor)
			]
		}

		return result
	}

}
