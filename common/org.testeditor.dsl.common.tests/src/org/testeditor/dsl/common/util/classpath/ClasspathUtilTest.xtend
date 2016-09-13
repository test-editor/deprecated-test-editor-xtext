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

import com.google.inject.AbstractModule
import com.google.inject.Guice
import java.io.File
import java.io.IOException
import java.io.OutputStream
import java.nio.file.Files
import java.util.List
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.Path
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder
import org.mockito.InjectMocks
import org.testeditor.dsl.common.testing.AbstractTest
import org.testeditor.dsl.common.util.MavenExecutor

import static org.mockito.Mockito.*

import static org.junit.Assume.*

class ClasspathUtilTest extends AbstractTest {

	@InjectMocks
	ClasspathUtil classpathUtil
	@Rule public TemporaryFolder tempFolder = new TemporaryFolder();

	@Test
	def void intTestGetBuildToolClasspathEntryWithMaven() {
		// given
		val targetDir = tempFolder.newFolder("target")
		tempFolder.newFile("pom.xml")
		val packageDir = new File(tempFolder.newFolder("src"), "/test/java/package")
		packageDir.mkdirs
		Files.write(new File(targetDir, "effective_pom.txt").toPath,
			new MavenClasspathUtilTest().getEffectiveTestPom(tempFolder.root, false).bytes)

		val intClasspathUtil = Guice.createInjector(getMavenExecutorMockModule()).getInstance(ClasspathUtil)

		// when
		val result = intClasspathUtil.getBuildToolClasspathEntry(new Path(packageDir.toString))

		// then
		assertEquals(new Path(tempFolder.root + "/src/test/java"), result)
	}

	def AbstractModule getMavenExecutorMockModule() {
		new AbstractModule() {

			override protected configure() {
				bind(MavenExecutor).to(ClasspathUtilTest.MavenExecutorDummy)
			}

		}
	}

	@Test
	def void intTestGetBuildToolClasspathEntryWithGradle() {
		assumeTrue(new GradleServerConnectUtil().canConnet())
		val gradleBuildFile = tempFolder.newFile("build.gradle")
		val packageDir = new File(tempFolder.newFolder("src"), "/test/java/package")
		packageDir.mkdirs

		Files.write(gradleBuildFile.toPath, getGradleBuildFileExample.bytes)
		val intClasspathUtil = Guice.createInjector().getInstance(ClasspathUtil)

		// when
		val result = intClasspathUtil.getBuildToolClasspathEntry(new Path(packageDir.toString))

		// then
		assertEquals(new Path(tempFolder.root + "/src/test/java"), result)
	}

	def private String getGradleBuildFileExample() {
		'''
			plugins {
			    id 'org.testeditor.gradle-plugin' version '0.3'
			    id 'maven'
			    id 'eclipse'
			}
			
			group = 'org.testeditor.demo'
			version = '1.0.0-SNAPSHOT'
			
			// In this section you declare where to find the dependencies of your project
			repositories {
			    jcenter()
			    maven { url "http://dl.bintray.com/test-editor/Fixtures" }
			    maven { url "http://dl.bintray.com/test-editor/test-editor-maven/" }
			}
			
			// Configure the testeditor plugin
			testeditor {
				version '1.1.0'
			}
			
			// In this section you declare the dependencies for your production and test code
			dependencies {
			    testCompile 'junit:junit:4.12'
			}
		'''
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

	static class MavenExecutorDummy extends MavenExecutor {

		override executeInNewJvm(String parameters, String pathToPom, String testParam, IProgressMonitor monitor,
			OutputStream outputStream, boolean useJvmClasspath) throws IOException {
			return 0;
		}

	}

}
