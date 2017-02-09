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
package org.testeditor.rcp4.tcltestrun

import java.io.File
import java.nio.file.Files
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.IPath
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.dsl.common.testing.AbstractTest

import static extension org.mockito.Mockito.*
import org.testeditor.dsl.common.util.MavenExecutor
import org.eclipse.core.runtime.IProgressMonitor
import java.io.OutputStream

class TclMavenLauncherTest extends AbstractTest {

	@InjectMocks TclMavenLauncher launcher
	@Mock MavenExecutor executor
	@Rule public TemporaryFolder tempFolder = new TemporaryFolder();
	
	@Test
	def void testGetProfiles() {
		// given
		val targetDir = tempFolder.newFolder("target")
		Files.write(new File(targetDir, "profiles.txt").toPath, '''Profile Id: foo (Active: true)
		Profile Id: bar (Active: false)'''.toString.bytes)
		val project = IProject.mock
		val path = IPath.mock
		when(path.toOSString).thenReturn(tempFolder.root.absolutePath)
		when(project.location).thenReturn(path)

		// when
		val profiles = launcher.getProfiles(project)

		// then 
		verify(executor).executeInNewJvm(any(String),any(String),any(String),any(IProgressMonitor),any(OutputStream))
		assertTrue(profiles.exists[it == "foo"])
		assertTrue(profiles.exists[it == "bar"])
		assertFalse(profiles.exists[it == "test"])
	}

}
