package org.testeditor.rcp4.tcltestrun

import java.io.File
import java.nio.file.Files
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.IPath
import org.junit.Test
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.dsl.common.testing.AbstractTest

import static extension org.mockito.Mockito.*
import org.junit.Rule
import org.junit.rules.TemporaryFolder

class TclMavenLauncherTest extends AbstractTest {

	@InjectMocks TclMavenLauncher launcher
	@Mock MavenExecutor executor
	@Rule public TemporaryFolder tempFolder = new TemporaryFolder();

	@Test
	def void testGetProfiles() {
		// given
		val targetDir = tempFolder.newFolder("target")
		if (!targetDir.exists) {
			targetDir.mkdir
		}
		Files.write(new File(targetDir, "profiles.txt").toPath, '''Profile Id: foo (Active: true)
		Profile Id: bar (Active: false)'''.toString.bytes)
		val project = IProject.mock
		val path = IPath.mock
		when(path.toOSString).thenReturn(tempFolder.root.absolutePath)
		when(project.location).thenReturn(path)

		// when
		val profiles = launcher.getProfiles(project)

		// then 
		assertTrue(profiles.exists[it == "foo"])
		assertTrue(profiles.exists[it == "bar"])
		assertFalse(profiles.exists[it == "test"])
	}

}
