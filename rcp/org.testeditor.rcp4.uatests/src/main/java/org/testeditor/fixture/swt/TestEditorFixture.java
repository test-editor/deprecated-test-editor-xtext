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
package org.testeditor.fixture.swt;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.e4.core.contexts.ContextInjectionFactory;
import org.eclipse.e4.core.contexts.IEclipseContext;
import org.eclipse.e4.ui.model.application.MApplication;
import org.eclipse.e4.ui.workbench.modeling.EModelService;
import org.eclipse.e4.ui.workbench.modeling.EPartService;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.testeditor.fixture.core.interaction.FixtureMethod;
import org.testeditor.rcp4.handlers.ResetUIHandler;
import org.testeditor.rcp4.handlers.SaveUIHandler;

/**
 * Fixture to enable Test-Editor specific operations to the tests.
 *
 */
public class TestEditorFixture {

	private static Logger logger = LogManager.getLogger(TestEditorFixture.class);

	/**
	 * Creates a simple Test-Editor project with some basic structures for
	 * testing the Test-Editor.
	 * 
	 * @throws Exception
	 *             on failure creating demo project
	 */
	@FixtureMethod
	public void createDemoProject() throws Exception {
		IProgressMonitor progressMonitor = new NullProgressMonitor();
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		IProject project = root.getProject("DemoProject");
		project.create(progressMonitor);
		project.open(progressMonitor);
	}

	/**
	 * Cleans the workspace. It deletes all projects in the workspace.
	 * 
	 * @throws Exception
	 *             on failure
	 */
	@FixtureMethod
	public void cleanWorkspace() throws Exception {
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		IProject[] projects = root.getProjects();
		for (IProject project : projects) {
			project.delete(true, new NullProgressMonitor());
		}
		root.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		logger.info("Workspace cleaned");
	}

	/**
	 * Save the UI perspective for the use in a later resetUI
	 */
	@FixtureMethod
	public void saveUI() {
		IWorkbench workbench = PlatformUI.getWorkbench();
		IEclipseContext ctx = workbench.getService(IEclipseContext.class);
		SaveUIHandler saveUIHandler = ContextInjectionFactory.make(SaveUIHandler.class, ctx);
		EModelService modelService = workbench.getService(EModelService.class);
		workbench.getDisplay().syncExec(() -> {
			saveUIHandler.saveUI(modelService);
		});
	}

	/**
	 * Resets the UI to a previously saved perspective
	 */
	@FixtureMethod
	public void resetUI() {
		IWorkbench workbench = PlatformUI.getWorkbench();
		IEclipseContext ctx = workbench.getService(IEclipseContext.class);
		ResetUIHandler resetUIHandler = ContextInjectionFactory.make(ResetUIHandler.class, ctx);
		EModelService modelService = workbench.getService(EModelService.class);
		EPartService partService = workbench.getService(EPartService.class);
		workbench.getDisplay().syncExec(() -> {
			resetUIHandler.resetUI(modelService, partService);
		});
	}

	@FixtureMethod
	public String isValidProject(String project) throws CoreException {
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		return Boolean.toString(root.getProject(project).hasNature("org.eclipse.jdt.core.javanature"));
	}

	@FixtureMethod
	public boolean containsWorkspaceFileText(String filePath, String searchText) throws CoreException, IOException {
		logger.info("Cearching for text {} in {}", searchText, filePath);
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		File file = new File(root.getLocation().toString(), filePath);
		return Files.lines(file.toPath()).anyMatch(s -> s.contains(searchText));
	}

}
