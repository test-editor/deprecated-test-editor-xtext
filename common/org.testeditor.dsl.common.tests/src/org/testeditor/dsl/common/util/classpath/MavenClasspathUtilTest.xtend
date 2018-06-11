/*******************************************************************************
 * Copyright (c) 2012 - 2018 Signal Iduna Corporation and others.
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
import org.eclipse.core.runtime.Path
import org.eclipse.xtext.util.StringInputStream
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder
import org.mockito.InjectMocks
import org.testeditor.dsl.common.testing.AbstractTest

class MavenClasspathUtilTest extends AbstractTest {

	@InjectMocks
	MavenClasspathUtil classpathUtil
	@Rule public TemporaryFolder tempFolder = new TemporaryFolder();

	@Test
	def void testReadMavenClasspathEntriesFromPom() {
		// given
		val stream = new StringInputStream(getEffectiveTestPom(tempFolder.root, false))
		val streamWithHelper = new StringInputStream(getEffectiveTestPom(tempFolder.root, true))

		// when
		val result = classpathUtil.readMavenClasspathEntriesFromPom(stream)
		val resultWithHelper = classpathUtil.readMavenClasspathEntriesFromPom(streamWithHelper)

		// then
		assertTrue(result.contains(new Path(tempFolder.root + "/src/main/java")))
		assertTrue(result.contains(new Path(tempFolder.root + "/src/test/java")))
		assertFalse(result.contains(new Path(tempFolder.root + "/src-gen")))
		assertFalse(result.contains(new Path(tempFolder.root + "/target/classes")))

		assertTrue(resultWithHelper.contains(new Path(tempFolder.root + "/src/main/java")))
		assertTrue(resultWithHelper.contains(new Path(tempFolder.root + "/src-gen")))
		assertFalse(resultWithHelper.contains(new Path(tempFolder.root + "/target/classes")))
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
			      <version>4.1.4</version>
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
