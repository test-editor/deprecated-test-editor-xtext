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

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.ui.PlatformUI;
import org.testeditor.fixture.core.interaction.FixtureMethod;

/**
 * Fixture to enable Test-Editor specific operations to the tests.
 *
 */
public class TestEditorFixture {

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

	@FixtureMethod
	public void cleanWorkspace() throws Exception {
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		IProject[] projects = root.getProjects();
		for (IProject project : projects) {
			project.delete(true, new NullProgressMonitor());
		}
	}

	@FixtureMethod
	public void restartApplication() throws Exception {
		PlatformUI.getWorkbench().restart();
	}

}
