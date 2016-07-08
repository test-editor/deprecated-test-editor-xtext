package org.testeditor.rcp4.tcltestrun

import java.io.File
import java.nio.file.Files
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.IPath
import org.junit.Test
import org.junit.runner.RunWith
import org.mockito.InjectMocks
import org.mockito.Mock
import org.mockito.runners.MockitoJUnitRunner

import static org.junit.Assert.*
import static org.mockito.Mockito.*

@RunWith(MockitoJUnitRunner)
class TclMavenLauncherTest {

	@Mock
	MavenExecutor executor

	@InjectMocks
	TclMavenLauncher launcher

	@Test
	def void testGetProfiles() {
		//given
		val targetDir = new File(System.getProperty("java.io.tmpdir"),"target")
		if(!targetDir.exists) {
			targetDir.mkdir
		}
		Files.write(new File(targetDir,"profiles.txt").toPath,'''Profile Id: foo (Active: true)
		Profile Id: bar (Active: false)'''.toString.bytes)
		val project = mock(IProject)
		val path = mock(IPath);
		when(path.toOSString).thenReturn(System.getProperty("java.io.tmpdir"))
		when(project.location).thenReturn(path)
		
		//when
		val profiles = launcher.getProfiles(project)
		
		//then findFirst[name == "MyApp"].assertNotNull
		assertTrue(profiles.exists[it == "foo"])
		assertTrue(profiles.exists[it == "bar"])
		assertFalse(profiles.exists[it == "test"])
	}

}
