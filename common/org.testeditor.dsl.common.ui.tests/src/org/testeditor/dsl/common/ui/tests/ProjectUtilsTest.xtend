package org.testeditor.dsl.common.ui.tests

import javax.inject.Inject
import org.eclipse.core.resources.IFolder
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.IProgressMonitor
import org.junit.Test
import org.mockito.Mock
import org.testeditor.aml.dsl.tests.AbstractTest
import org.testeditor.dsl.common.ui.utils.ProjectUtils

import static org.mockito.Matchers.*
import static org.mockito.Mockito.*

class ProjectUtilsTest extends AbstractTest {
	@Mock var IProject project
	@Mock var IFolder iFolder1
	@Mock var IFolder iFolder2
	@Mock var IFolder iFolder3
	@Inject var ProjectUtils classUnderTest

	@Test
	def void testCreateOrUpdate_CreateOnlyLast() {
		// given
		when(project.getFolder("src")).thenReturn(iFolder1)
		when(iFolder1.getFolder("folder")).thenReturn(iFolder2)
		when(iFolder2.getFolder("nextfolder")).thenReturn(iFolder3)

		when(iFolder1.exists).thenReturn(true)
		when(iFolder2.exists).thenReturn(true)
		when(iFolder3.exists).thenReturn(false)

		// when
		classUnderTest.createOrGetDeepFolder(project, "src/folder/nextfolder")

		// then
		verify(iFolder1, never).create(eq(true), eq(false), any(IProgressMonitor))
		verify(iFolder2, never).create(eq(true), eq(false), any(IProgressMonitor))
		verify(iFolder3).create(eq(true), eq(false), any(IProgressMonitor))
	}

	@Test
	def void testCreateOrUpdate_CreateAll() {
		// given
		when(project.getFolder("src")).thenReturn(iFolder1)
		when(iFolder1.getFolder("folder")).thenReturn(iFolder2)
		when(iFolder2.getFolder("nextfolder")).thenReturn(iFolder3)

		when(iFolder1.exists).thenReturn(false)
		when(iFolder2.exists).thenReturn(false)
		when(iFolder3.exists).thenReturn(false)

		// when
		classUnderTest.createOrGetDeepFolder(project, "src/folder/nextfolder")

		// then
		verify(iFolder1).create(eq(true), eq(false), any(IProgressMonitor))
		verify(iFolder2).create(eq(true), eq(false), any(IProgressMonitor))
		verify(iFolder3).create(eq(true), eq(false), any(IProgressMonitor))
	}

	@Test
	def void testGetDeepFolder_CreateNone() {
		// given
		when(project.getFolder("src")).thenReturn(iFolder1)
		when(iFolder1.getFolder("folder")).thenReturn(iFolder2)
		when(iFolder2.getFolder("nextfolder")).thenReturn(iFolder3)

		when(iFolder1.exists).thenReturn(false)
		when(iFolder2.exists).thenReturn(false)
		when(iFolder3.exists).thenReturn(false)

		// when
		val result = classUnderTest.getDeepFolder(project, "src/folder/nextfolder")

		// then
		result.assertNull

		verify(iFolder1, never).create(eq(true), eq(false), any(IProgressMonitor))
		verify(iFolder2, never).create(eq(true), eq(false), any(IProgressMonitor))
		verify(iFolder3, never).create(eq(true), eq(false), any(IProgressMonitor))
	}

	@Test(expected=RuntimeException)
	def void testGetDeepFolder_ExceptionNoFolderGiven() {
		// when
		classUnderTest.getDeepFolder(project, "") // no folder
	}

	@Test(expected=RuntimeException)
	def void testGetDeepFolder_ExceptionTrailingSlash() {
		// when
		classUnderTest.getDeepFolder(project, "src/folder/nextfolder/") // trailing slash
	}

	@Test(expected=RuntimeException)
	def void testGetDeepFolder_ExceptionPreceedingSlash() {
		// when
		classUnderTest.getDeepFolder(project, "/src/folder/nextfolder") // preceeding slash
	}

	@Test
	def void testGetDeepFolder_YieldLastFolder() {
		// given
		when(project.getFolder("src")).thenReturn(iFolder1)
		when(iFolder1.getFolder("folder")).thenReturn(iFolder2)
		when(iFolder2.getFolder("nextfolder")).thenReturn(iFolder3)

		when(iFolder1.exists).thenReturn(true)
		when(iFolder2.exists).thenReturn(true)
		when(iFolder3.exists).thenReturn(true)

		// when
		val result = classUnderTest.getDeepFolder(project, "src/folder/nextfolder")

		// then
		assertEquals(result, iFolder3)

		verify(iFolder1, never).create(eq(true), eq(false), any(IProgressMonitor))
		verify(iFolder2, never).create(eq(true), eq(false), any(IProgressMonitor))
		verify(iFolder3, never).create(eq(true), eq(false), any(IProgressMonitor))
	}

	@Test
	def void testGetDeepFolder_OneFolder() {
		// given
		when(project.getFolder("src")).thenReturn(iFolder1)

		when(iFolder1.exists).thenReturn(true)

		// when
		val result = classUnderTest.getDeepFolder(project, "src")

		// then
		assertEquals(result, iFolder1)

		verify(iFolder1, never).create(eq(true), eq(false), any(IProgressMonitor))
	}
}
