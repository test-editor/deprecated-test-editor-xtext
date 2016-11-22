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
package org.testeditor.rcp4.views.projectexplorer

import org.eclipse.core.resources.IFolder
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.IWorkspaceRoot
import org.eclipse.core.runtime.IPath
import org.eclipse.jdt.core.IClasspathEntry
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jdt.core.JavaCore
import org.junit.Before
import org.junit.Test
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.dsl.common.testing.AbstractTest
import org.testeditor.dsl.common.util.WorkspaceHelper

import static org.mockito.Matchers.*
import static org.mockito.Mockito.*

class TEContentProviderTest extends AbstractTest {

	@InjectMocks TEContentProvider contentProvider
	
	@Mock WorkspaceHelper workspaceHelper
	@Mock JavaCoreHelper javaCoreHelper
	@Mock IClasspathEntry relevantClasspathSource
	@Mock IClasspathEntry irrelevantClasspathSource
	@Mock IWorkspaceRoot root
	@Mock IProject project
	@Mock IFolder folder
	@Mock IJavaProject javaProject
	
	@Before
	def void setupJavaProject() {
		when(javaCoreHelper.create(any)).thenReturn(javaProject)
		when(project.hasNature(JavaCore.NATURE_ID)).thenReturn(true)
	}
	
	@Before
	def void setupRoot() {
		when(workspaceHelper.root).thenReturn(root)
		when(root.getFolder(any)).thenReturn(folder)
	}

	@Before
	def void setupRelevantClasspathSources() {
		when(relevantClasspathSource.entryKind).thenReturn(IClasspathEntry.CPE_SOURCE)
		val relevantPath = mock(IPath)
		when(relevantClasspathSource.path).thenReturn(relevantPath)
		when(relevantPath.segments).thenReturn(#["src", "main", "java"])
	}

	@Before
	def void setupIrrelevantClasspathSources() {
		when(irrelevantClasspathSource.entryKind).thenReturn(IClasspathEntry.CPE_SOURCE)
		val irrelevantPath = mock(IPath)
		when(irrelevantClasspathSource.path).thenReturn(irrelevantPath)
		when(irrelevantPath.segments).thenReturn(#["src-gen", "main", "java"]) // <-- src-gen makes it irrelevant
	}

	@Test
	def void testGetChildrenOfProject() {
		// given
		val classPathOther = mock(IClasspathEntry)
		when(classPathOther.entryKind).thenReturn(IClasspathEntry.CPE_LIBRARY)
		when(javaProject.rawClasspath).thenReturn(#[relevantClasspathSource, irrelevantClasspathSource, classPathOther])

		// when
		val cpEntries = contentProvider.getChildren(project)

		// then
		assertNotNull(cpEntries)
		cpEntries.assertSize(1)
		cpEntries.head.assertEquals(relevantClasspathSource)
	}

	@Test
	def void testGetChildrenOfClasspathEntry() {
		// given
		val resource = mock(IResource)
		when(folder.members).thenReturn(#[resource])

		// when
		val children = contentProvider.getChildren(relevantClasspathSource)

		// then
		children.assertSize(1)
		children.head.assertEquals(resource)
	}

	@Test
	def void testGetParentOfResource() {
		// given
		when(javaProject.rawClasspath).thenReturn(#[relevantClasspathSource])
		val resource = mock(IResource)
		when(folder.members).thenReturn(#[resource])		
		when(resource.project).thenReturn(project)

		// when
		val parent = contentProvider.getParent(resource)

		// then
		parent.assertEquals(relevantClasspathSource)
	}

	@Test
	def void testGetParentOfClasspathEntry() {
		// given
		when(folder.project).thenReturn(project)

		// when
		val parent = contentProvider.getParent(relevantClasspathSource)

		// then
		parent.assertEquals(project)
	}

}
