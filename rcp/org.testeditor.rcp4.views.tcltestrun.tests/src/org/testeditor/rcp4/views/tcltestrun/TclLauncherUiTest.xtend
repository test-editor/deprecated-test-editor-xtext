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
package org.testeditor.rcp4.views.tcltestrun

import com.google.inject.Injector
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IFolder
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.e4.core.contexts.EclipseContextFactory
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.jface.operation.IRunnableWithProgress
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.xtext.naming.QualifiedName
import org.junit.Before
import org.junit.Test
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.dsl.common.testing.AbstractTest
import org.testeditor.dsl.common.ui.utils.ProgressMonitorRunner
import org.testeditor.dsl.common.util.EclipseContextHelper
import org.testeditor.rcp4.tcltestrun.TclGradleLauncher
import org.testeditor.tcl.dsl.ui.testlaunch.LaunchShortcutUtil
import org.testeditor.tcl.dsl.ui.util.TclIndexHelper
import org.testeditor.tcl.dsl.ui.util.TclInjectorProvider

import static org.mockito.Matchers.*

import static extension org.mockito.Mockito.*

class TclLauncherUiTest extends AbstractTest {

	@Mock
	TclInjectorProvider tclInjectorProvider

	@Mock
	IStructuredSelection selection

	@Mock
	EclipseContextHelper eclipseContextHelper

	@Mock
	LaunchShortcutUtil launchUtil

	@Mock
	TclIndexHelper tclIndexHelper

	@Mock
	Injector injector

	@Mock
	TclGradleLauncher gradleLauncher

	@Mock
	ProgressMonitorRunner progressRunner

	@InjectMocks
	TclLauncherUi launcherUi

	@Before
	def void setup() {
		// given
		when(tclInjectorProvider.get).thenReturn(injector)
		when(injector.getInstance(LaunchShortcutUtil)).thenReturn(launchUtil)
	}

	@Test
	def void testGetTestCaseListOnOneSelection() {
		// given
		val resource = IResource.mock
		when(selection.firstElement).thenReturn(resource)
		when(resource.fileExtension).thenReturn("tcl")
		when(launchUtil.getQualifiedNameForTestInTcl(any)).thenReturn(QualifiedName.create("mypackage", "myTest"))

		// when
		val testCases = launcherUi.createTestCasesList(selection)

		// then
		assertEquals(1, testCases.size)
		assertTrue(testCases.exists[it == "mypackage.myTest"])
	}

	@Test
	def void testGetTestCaseListOnManySelection() {
		// given
		val resource1 = IResource.mock
		val resource2 = IResource.mock
		when(launchUtil.getQualifiedNameForTestInTcl(resource1)).thenReturn(QualifiedName.create("mypackage", "myTest"))
		when(launchUtil.getQualifiedNameForTestInTcl(resource2)).thenReturn(
			QualifiedName.create("mypackage", "mySecondTest"))
		when(selection.size).thenReturn(2)
		when(selection.toList).thenReturn(#[resource1, resource2])
		when(resource1.fileExtension).thenReturn("tcl")
		when(resource2.fileExtension).thenReturn("tcl")

		// when
		val testCases = launcherUi.createTestCasesList(selection)

		// then
		assertEquals(2, testCases.size)
		assertTrue(testCases.exists[it == "mypackage.myTest"])
		assertTrue(testCases.exists[it == "mypackage.mySecondTest"])
	}

	@Test
	def void testGetTestCaseListOnFolderSelection() {
		// given
		val folder = IFolder.mock
		val tc1 = createTestCaseMockFile("mypackage", "myTest1")
		val tc2 = createTestCaseMockFile("mypackage", "myTest2")
		val tc3 = createTestCaseMockFile("mysubpackage", "myTest3")
		val otherFile = IFile.mock
		val subFolder = IFolder.mock
		when(selection.firstElement).thenReturn(folder)
		when(otherFile.fileExtension).thenReturn("tsl")
		when(folder.members).thenReturn(#[tc1, tc2, otherFile, subFolder]);
		when(subFolder.members).thenReturn(#[tc3])

		// when
		val testCases = launcherUi.createTestCasesList(selection)

		// then
		assertEquals(3, testCases.size)
		assertTrue(testCases.exists[it == "mypackage.myTest1"])
		assertTrue(testCases.exists[it == "mypackage.myTest2"])
		assertTrue(testCases.exists[it == "mysubpackage.myTest3"])
	}

	def IFile createTestCaseMockFile(String packageName, String testName) {
		val result = IFile.mock
		when(result.fileExtension).thenReturn("tcl")
		when(launchUtil.getQualifiedNameForTestInTcl(result)).thenReturn(QualifiedName.create(packageName, testName))
		return result
	}

	@Test
	def void testCreateGradleTestCasesList() {
		// given
		createSelectionForMavenStructure()

		// when
		val testCases = launcherUi.createGradleTestCasesList(selection)

		// then		
		assertEquals(1, testCases.size)
		assertTrue(testCases.exists[it == "mypackage*"])
	}

	def createSelectionForMavenStructure() {
		val folder = IFolder.mock
		val javaElement = IJavaElement.mock
		when(selection.toList).thenReturn(#[folder])
		when(folder.getAdapter(IJavaElement)).thenReturn(javaElement)
		when(javaElement.elementName).thenReturn("mypackage")
	}

	@Test
	def void testStoreLastTestLaunch() {
		// given
		val project = IProject.mock
		val buildFile = IFile.mock
		val context = EclipseContextFactory.create
		when(eclipseContextHelper.eclipseContext).thenReturn(context)
		when(project.getFile("build.gradle")).thenReturn(buildFile)
		when(buildFile.exists).thenReturn(true)
		createSelectionForMavenStructure

		// when
		launcherUi.launch(selection, project, null, false)

		// then
		assertTrue(context.containsKey(TclLauncherUi))
		assertTrue(context.containsKey(TestLaunchInformation))
		val lasTestLaunch = context.get(TestLaunchInformation)
		assertTrue(lasTestLaunch.testCasesCommaList.contains("mypackage*"))
		verify(tclIndexHelper).createTestCaseIndex
		verify(progressRunner).run(any(IRunnableWithProgress))
	}

}
