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
package org.testeditor.dsl.common.util.classpath

import java.io.File
import java.nio.file.Files
import java.util.List
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.junit.Ignore
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder
import org.mockito.InjectMocks
import org.testeditor.dsl.common.testing.AbstractTest

import static org.mockito.Mockito.*

class ClasspathUtilTest extends AbstractTest {
	
	@InjectMocks
	ClasspathUtil classpathUtil
	@Rule public TemporaryFolder tempFolder = new TemporaryFolder();

	@Test
	@Ignore
	//Disabled because it is an integration test. The setup of mock and real classes needs further work.
	//TODO Fix the test.
	def void testGetBuildToolClasspathEntryWithMaven() {
		// given
		val targetDir = tempFolder.newFolder("target")
		tempFolder.newFile("pom.xml")
		val packageDir = new File(tempFolder.newFolder("src"), "/test/java/package")
		packageDir.mkdirs
		Files.write(new File(targetDir, "effective_pom.txt").toPath,
			new MavenClasspathUtilTest().getEffectiveTestPom(tempFolder.root, false).bytes)

		// when
		val result = classpathUtil.getBuildToolClasspathEntry(new Path(packageDir.toString))

		// then
		assertEquals(new Path(tempFolder.root + "/src/test/java"), result)
	}

	@Test
	def void testGetBuildProjectBaseDir() {
		// given
		val path = getPathForBuildFileSearch(#[])
		val basePathWithPom = getPathForBuildFileSearch(#["pom.xml"])
		val basePathWithGradle = getPathForBuildFileSearch(#["build.gradle"])

		// when
		val buildScriptNotFound = classpathUtil.getBuildProjectBaseDir(path)
		val pathWithPom = classpathUtil.getBuildProjectBaseDir(basePathWithPom)
		val pathWithGradle = classpathUtil.getBuildProjectBaseDir(basePathWithGradle)

		// then
		assertNull(buildScriptNotFound)
		assertSame(basePathWithPom, pathWithPom)
		assertSame(basePathWithGradle, pathWithGradle)
	}

	def IPath getPathForBuildFileSearch(List<String> objects) {
		val path = mock(IPath)
		val folder = mock(File)
		when(path.toFile).thenReturn(folder)
		when(folder.list).thenReturn(objects)
		when(folder.parent).thenReturn(null)
		return path
	}

}
