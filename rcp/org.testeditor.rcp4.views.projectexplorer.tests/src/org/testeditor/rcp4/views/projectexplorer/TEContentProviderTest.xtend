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

import org.eclipse.core.resources.IProject
import org.eclipse.jdt.core.JavaCore
import org.junit.Test
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.dsl.common.testing.AbstractTest

import static org.mockito.Mockito.*
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jdt.core.IClasspathEntry
import org.eclipse.core.resources.IWorkspaceRoot
import org.eclipse.core.resources.IFolder
import org.eclipse.core.resources.IResource

class TEContentProviderTest extends AbstractTest {

	@InjectMocks TEContentProvider contentProvider
	@Mock WorkspaceRootHelper workspaceHelper
	@Mock JavaCoreHelper javaCoreHelper

	@Test
	def void testGetChildrenOfProject() {
		// given
		val parent = mock(IProject)
		val classPathSource = mock(IClasspathEntry)
		val classPathOther = mock(IClasspathEntry)
		val javaProject = mock(IJavaProject)
		contentProvider.javaCoreHelper = javaCoreHelper
		when(javaCoreHelper.create(any)).thenReturn(javaProject)
		when(parent.hasNature(JavaCore.NATURE_ID)).thenReturn(true)
		when(classPathSource.entryKind).thenReturn(IClasspathEntry.CPE_SOURCE)
		when(classPathOther.entryKind).thenReturn(IClasspathEntry.CPE_LIBRARY)
		when(javaProject.rawClasspath).thenReturn(#[classPathSource, classPathOther])

		// when
		val cpEntries = contentProvider.getChildren(parent)

		// then
		assertNotNull(cpEntries)
		assertEquals(1, cpEntries.size)
	}

	@Test
	def void testGetChildrenOfClasspathEntry() {
		// given
		val parent = mock(IClasspathEntry)
		val root = mock (IWorkspaceRoot)
		val folder = mock(IFolder)
		val resource = mock(IResource)
		when(workspaceHelper.root).thenReturn(root)
		when(root.getFolder(any)).thenReturn(folder)
		when(folder.members).thenReturn(#[resource])
		
		// when
		val childs = contentProvider.getChildren(parent)

		// then
		assertTrue(childs.contains(resource))
	}

	@Test
	def void testGetParentOfResource() {
		// given
		val resource = mock(IResource)
		val project = mock(IProject)
		val root = mock (IWorkspaceRoot)
		val classPathSource = mock(IClasspathEntry)
		val javaProject = mock(IJavaProject)
		val folder = mock(IFolder)
		contentProvider.javaCoreHelper = javaCoreHelper
		when(javaCoreHelper.create(any)).thenReturn(javaProject)
		when(project.hasNature(JavaCore.NATURE_ID)).thenReturn(true)
		when(classPathSource.entryKind).thenReturn(IClasspathEntry.CPE_SOURCE)
		when(javaProject.rawClasspath).thenReturn(#[classPathSource])
		when(resource.project).thenReturn(project)
		when(workspaceHelper.root).thenReturn(root)
		when(root.getFolder(any)).thenReturn(folder)
		when(folder.members).thenReturn(#[resource])
		
		// when
		val parent = contentProvider.getParent(resource)

		// then
		assertEquals(classPathSource, parent)
	}

	@Test
	def void testGetParentOfClasspathEntry() {
		// given
		val cpEntry = mock(IClasspathEntry)
		val root = mock (IWorkspaceRoot)
		val folder = mock(IFolder)
		val project = mock(IProject)
		when(workspaceHelper.root).thenReturn(root)
		when(root.getFolder(any)).thenReturn(folder)
		when(folder.project).thenReturn(project)
		
		// when
		val parent = contentProvider.getParent(cpEntry)

		// then
		assertEquals(project, parent)
	}

}
