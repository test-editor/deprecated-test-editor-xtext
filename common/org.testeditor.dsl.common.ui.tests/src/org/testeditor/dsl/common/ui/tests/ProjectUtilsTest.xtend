package org.testeditor.dsl.common.ui.tests

import javax.inject.Inject
import org.eclipse.core.resources.IFolder
import org.eclipse.core.resources.IProject
import org.junit.Before
import org.junit.Test
import org.mockito.Mock
import org.testeditor.aml.dsl.tests.AbstractTest
import org.testeditor.dsl.common.ui.utils.ProjectUtils

import static extension org.mockito.Matchers.*
import static extension org.mockito.Mockito.*

class ProjectUtilsTest extends AbstractTest {

	@Inject ProjectUtils projectUtils // class under test
	@Mock IProject project
	@Mock IFolder srcFolder
	@Mock IFolder nextFolder
	@Mock IFolder lastFolder

	@Before
	override setUp() {
		super.setUp
		
		// folder mock setup
		when(project.getFolder("src")).thenReturn(srcFolder)
		when(srcFolder.getFolder("next")).thenReturn(nextFolder)
		when(nextFolder.getFolder("last")).thenReturn(lastFolder)

		// no folder existing
		when(srcFolder.exists).thenReturn(false)
		when(nextFolder.exists).thenReturn(false)
		when(lastFolder.exists).thenReturn(false)
	}

	@Test
	def void testCreateOrUpdate_CreateOnlyLast() {
		// given
		when(srcFolder.exists).thenReturn(true)
		when(nextFolder.exists).thenReturn(true)

		// when
		projectUtils.createOrGetDeepFolder(project, "src/next/last")

		// then
		srcFolder.verify(never).create(true.eq, false.eq, any)
		nextFolder.verify(never).create(true.eq, false.eq, any)
		lastFolder.verify.create(true.eq, false.eq, any)
	}

	@Test
	def void testCreateOrUpdate_CreateAll() {
		// given
		
		// when
		projectUtils.createOrGetDeepFolder(project, "src/next/last")

		// then
		srcFolder.verify.create(true.eq, false.eq, any)
		nextFolder.verify.create(true.eq, false.eq, any)
		lastFolder.verify.create(true.eq, false.eq, any)
	}

	@Test
	def void testGetDeepFolder_CreateNone() {
		// given
		
		// when
		val result = projectUtils.getDeepFolder(project, "src/next/last")

		// then
		result.assertNull

		srcFolder.verify(never).create(true.eq, false.eq, any)
		nextFolder.verify(never).create(true.eq, false.eq, any)
		lastFolder.verify(never).create(true.eq, false.eq, any)
	}

	@Test(expected=RuntimeException)
	def void testGetDeepFolder_ExceptionNoFolderGiven() {
		// when
		projectUtils.getDeepFolder(project, "") // no folder
	}

	@Test(expected=RuntimeException)
	def void testGetDeepFolder_ExceptionTrailingSlash() {
		// when
		projectUtils.getDeepFolder(project, "src/folder/nextfolder/") // trailing slash
	}

	@Test(expected=RuntimeException)
	def void testGetDeepFolder_ExceptionPreceedingSlash() {
		// when
		projectUtils.getDeepFolder(project, "/src/folder/nextfolder") // preceeding slash
	}

	@Test
	def void testGetDeepFolder_YieldLastFolder() {
		// given
		when(srcFolder.exists).thenReturn(true)
		when(nextFolder.exists).thenReturn(true)
		when(lastFolder.exists).thenReturn(true)

		// when
		val result = projectUtils.getDeepFolder(project, "src/next/last")

		// then
		result.assertEquals(lastFolder)

		srcFolder.verify(never).create(true.eq, false.eq, any)
		nextFolder.verify(never).create(true.eq, false.eq, any)
		lastFolder.verify(never).create(true.eq, false.eq, any)
	}

	@Test
	def void testGetDeepFolder_OneFolder() {
		// given
		when(project.getFolder("src")).thenReturn(srcFolder)

		when(srcFolder.exists).thenReturn(true)

		// when
		val result = projectUtils.getDeepFolder(project, "src")

		// then
		result.assertEquals(srcFolder)

		srcFolder.verify(never).create(true.eq, false.eq, any)
	}
}
