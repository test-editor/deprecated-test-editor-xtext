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
package org.testeditor.dsl.common.ui.wizards

import javax.inject.Inject
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.jface.wizard.IWizardPage
import org.eclipse.jface.wizard.WizardPage
import org.eclipse.ui.IWorkbench
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage
import org.eclipse.ui.ide.IDE
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard
import org.testeditor.dsl.common.ui.utils.ProgressMonitorRunner
import org.testeditor.dsl.common.ui.utils.ProjectContentGenerator

/** 
 * Wizard to create a new test project. 
 *  - add java nature
 *  - add xtext nature
 *  - add build system nature
 */
class NewProjectWizard extends BasicNewProjectResourceWizard {

	TestProjectConfigurationWizardPage configPage

	@Inject ProjectContentGenerator projectContentGenerator
	@Inject ProgressMonitorRunner progressMonitorRunner

	override init(IWorkbench bench, IStructuredSelection selection) {
		super.init(bench, selection)
		windowTitle = "Create test-first project"
	}

	override addPages() {
		super.addPages
		configPage = new TestProjectConfigurationWizardPage("configPage") => [
			availableBuildSystems = projectContentGenerator.availableBuildSystems
			availableFixtureNames = projectContentGenerator.availableFixtureNames
			addPage
		]
	}

	override getNextPage(IWizardPage page) {
		return configPage
	}

	override canFinish() {
		val projectName = (pages.head as WizardNewProjectCreationPage).projectName
		val nameOk = projectName.matches("^[a-zA-Z\\._0-9]+$")
		if (!nameOk && !projectName.empty) {
			(pages.head as WizardPage).errorMessage = "project name may contain A-Z, 0-9, dots and underscore only"
		}
		return super.canFinish && nameOk
	}

	override performFinish() {
		val result = super.performFinish()

		val selectedFixtures = configPage.selectedFixtures
		val buildSystemName = configPage.buildSystemName
		val withDemoCode = configPage.withDemoCode
		progressMonitorRunner.run [ monitor |
			projectContentGenerator.createProjectContent(newProject, selectedFixtures, buildSystemName, withDemoCode, monitor)
		]
		val demoTest = projectContentGenerator.demoTclFile
		if (demoTest !== null) {
			val activePage = PlatformUI.workbench.activeWorkbenchWindow.activePage
			IDE.openEditor(activePage, demoTest)
		}
		return result
	}

}
