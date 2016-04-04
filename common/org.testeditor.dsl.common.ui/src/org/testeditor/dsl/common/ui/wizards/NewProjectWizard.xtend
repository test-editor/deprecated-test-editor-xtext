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
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.jface.wizard.IWizardPage
import org.eclipse.ui.IWorkbench
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard
import org.eclipse.xtext.ui.XtextProjectHelper
import org.testeditor.dsl.common.ide.util.ProjectContentGenerator
import org.testeditor.dsl.common.ui.utils.ProgressMonitorRunner
import org.testeditor.dsl.common.ui.utils.ProjectUtils

/** 
 * Wizard to create a new test project. 
 *  - add java nature
 *  - add xtext nature
 *  - add build system nature
 */
class NewProjectWizard extends BasicNewProjectResourceWizard {

	@Inject extension ProjectUtils
	TestProjectConfigurationWizardPage configPage

	@Inject ProjectContentGenerator projectContentGenerator
	@Inject ProgressMonitorRunner progressMonitorRunner

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

		newProject.createOrGetDeepFolder(ProjectContentGenerator.SRC_FOLDER)
		newProject.createOrGetDeepFolder(ProjectContentGenerator.SRC_TEST_FOLDER)
		newProject.createOrGetDeepFolder(ProjectContentGenerator.SRC_TCL_TEST_FOLDER)
		newProject.addNature(JavaCore.NATURE_ID)
		JavaCore.create(newProject)
		newProject.addNature(XtextProjectHelper.NATURE_ID)
		val selectedFixtures = configPage.selectedFixtures
		val buildSystemName = configPage.buildSystemName
		val withDemoCode = configPage.withDemoCode
		if (!configPage.selectedFixtures.isEmpty) {
			progressMonitorRunner.run [ monitor |
				projectContentGenerator.createProjectContent(newProject, selectedFixtures, buildSystemName,
					withDemoCode, monitor)
			]
		}
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
