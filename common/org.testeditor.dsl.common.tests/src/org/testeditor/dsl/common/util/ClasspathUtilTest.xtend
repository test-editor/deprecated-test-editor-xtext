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
package org.testeditor.dsl.common.util

import java.io.File
import java.nio.file.Files
import java.util.List
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.eclipse.xtext.util.StringInputStream
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.dsl.common.testing.AbstractTest

import static org.mockito.Mockito.*

class ClasspathUtilTest extends AbstractTest {

	@Mock
	MavenCommand mavenCommand
	@InjectMocks
	ClasspathUtil classpathUtil
	@Rule public TemporaryFolder tempFolder = new TemporaryFolder();

	@Test
	def void testGetBuildToolClasspathEntryWithMaven() {
		// given
		val targetDir = tempFolder.newFolder("target")
		tempFolder.newFile("pom.xml")
		val packageDir = new File(tempFolder.newFolder("src"), "/test/java/package")
		packageDir.mkdirs
		Files.write(new File(targetDir, "effective_pom.txt").toPath, getEffectiveTestPom(tempFolder.root, false).bytes)

		// when
		val result = classpathUtil.getBuildToolClasspathEntry(new Path(packageDir.toString))

		// then
		verify(mavenCommand).execute(any(File), any(String), any(String))
		assertEquals(tempFolder.root + "/src/test/java", result.toString)
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

	@Test
	def void testReadMavenClasspathEntriesFromPom() {
		// given
		val stream = new StringInputStream(getEffectiveTestPom(tempFolder.root, false))
		val streamWithHelper = new StringInputStream(getEffectiveTestPom(tempFolder.root, true))

		// when
		val result = classpathUtil.readMavenClasspathEntriesFromPom(stream)
		val resultWithHelper = classpathUtil.readMavenClasspathEntriesFromPom(streamWithHelper)

		// then
		val paths = result.map[toString]
		val pathsWithHelper = resultWithHelper.map[toString]

		assertTrue(paths.contains(tempFolder.root + "/src/main/java"))
		assertTrue(paths.contains(tempFolder.root + "/src/test/java"))
		assertFalse(paths.contains(tempFolder.root + "/src-gen"))
		assertFalse(paths.contains(tempFolder.root + "/target/classes"))

		assertTrue(pathsWithHelper.contains(tempFolder.root + "/src/main/java"))
		assertTrue(pathsWithHelper.contains(tempFolder.root + "/src-gen"))
		assertFalse(pathsWithHelper.contains(tempFolder.root + "/target/classes"))
	}

	def String getEffectiveTestPom(File prjDir, boolean withHelperPlugIn) {
		'''
			<?xml version="1.0" encoding="UTF-8"?>
			<!-- ====================================================================== -->
			<!--                                                                        -->
			<!-- See: http://maven.apache.org/plugins/maven-help-plugin/                -->
			<!--                                                                        -->
			<!-- ====================================================================== -->
			
			<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
			  <modelVersion>4.0.0</modelVersion>
			  <parent>
			    <groupId>org.testeditor</groupId>
			    <artifactId>org.testeditor.releng.parent</artifactId>
			    <version>1.2.0-SNAPSHOT</version>
			    <relativePath>../../releng/org.testeditor.releng.parent</relativePath>
			  </parent>
			  <groupId>org.testeditor</groupId>
			  <artifactId>org.testeditor.rcp4.uatests</artifactId>
			  <version>1.2.0-SNAPSHOT</version>
			  <packaging>eclipse-test-plugin</packaging>
			  <dependencies>
			    <dependency>
			      <groupId>org.testeditor.fixture</groupId>
			      <artifactId>core-fixture</artifactId>
			      <version>2.5.2</version>
			      <scope>compile</scope>
			    </dependency>
			  </dependencies>
			  <build>
			    <sourceDirectory>«prjDir.toString»/src/main/java</sourceDirectory>
			    <scriptSourceDirectory>«prjDir.toString»/src/main/scripts</scriptSourceDirectory>
			    <testSourceDirectory>«prjDir.toString»/src/test/java</testSourceDirectory>
			    <outputDirectory>«prjDir.toString»/target/classes</outputDirectory>
			    <testOutputDirectory>«prjDir.toString»/target/test-classes</testOutputDirectory>
			    <resources>
			      <resource>
			        <directory>«prjDir.toString»/src/main/resources</directory>
			      </resource>
			    </resources>
			    <testResources>
			      <testResource>
			        <directory>«prjDir.toString»/src/test/resources</directory>
			      </testResource>
			    </testResources>
			    <directory>«prjDir.toString»/target</directory>
			    «IF (withHelperPlugIn)»
			    	<plugins>
			    	  <plugin>
			    	    <groupId>org.codehaus.mojo</groupId>
			    	    <artifactId>build-helper-maven-plugin</artifactId>
			    	    <version>1.7</version>
			    	    <executions>
			    	      <execution>
			    	        <id>add-source</id>
			    	        <phase>generate-sources</phase>
			    	        <goals>
			    	          <goal>add-source</goal>
			    	        </goals>
			    	        <configuration>
			    	          <sources>
			    	            <source>«prjDir.toString»/src-gen</source>
			    	          </sources>
			    	        </configuration>
			    	      </execution>
			    	    </executions>
			    	  </plugin>
			    	</plugins>
			    «ENDIF»
			  </build>
			</project>
		'''
	}

}
