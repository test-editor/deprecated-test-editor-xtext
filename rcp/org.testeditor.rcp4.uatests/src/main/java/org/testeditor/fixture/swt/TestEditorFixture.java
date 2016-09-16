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
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.testeditor.fixture.core.interaction.FixtureMethod;

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
		logger.info("Workspace cleaned");
	}

	/**
	 * Resets the UI of the application to the initial state.
	 */
	@FixtureMethod
	public void resetApplication() {
		IWorkbench workbench = PlatformUI.getWorkbench();
		// TODO fix the perspective!
		// IWorkbenchWindow[] workbenchWindows =
		// workbench.getWorkbenchWindows();
		// logger.info("Resetting {} workbench window(s).",
		// workbenchWindows.length);
		// workbench.getDisplay().syncExec(() -> {
		// for (IWorkbenchWindow window : workbenchWindows) {
		// window.getActivePage().closeAllEditors(true);
		// window.getActivePage().resetPerspective();
		// }
		// });
		// hack until perspective is fixed: bring project explorer to the front
		final IWorkbenchWindow window = workbench.getWorkbenchWindows()[0];
		workbench.getDisplay().syncExec(() -> {
			String viewToOpen = "org.testeditor.rcp4.views.ProjectExplorer";
			try {
				window.getActivePage().showView(viewToOpen);
			} catch (PartInitException e) {
				logger.error("Could not init view='{}'.", viewToOpen);
			}
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
