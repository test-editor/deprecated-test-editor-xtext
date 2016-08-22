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

import org.eclipse.xtext.util.StringInputStream
import org.junit.Test
import org.mockito.InjectMocks
import org.testeditor.dsl.common.testing.AbstractTest

class ClasspathUtilTest extends AbstractTest {
	
	@InjectMocks
	ClasspathUtil classpathUtil
	
	@Test
	def void testReadMavenClasspathEntriesFromPom() {
		// given
		val stream = new StringInputStream(effectiveTestPom)
		
		// when
		val result = classpathUtil.readMavenClasspathEntriesFromPom(stream)
		
		// then
		val paths = result.map[toString]
		assertTrue(paths.contains("/home/user/projects/te/rcp/org.testeditor.rcp4.uatests/src/main/java"))
		assertTrue(paths.contains("/home/user/projects/te/rcp/org.testeditor.rcp4.uatests/src/test/java"))
		assertFalse(paths.contains("/home/user/projects/te/rcp/org.testeditor.rcp4.uatests/target/classes"))
	}

	def String getEffectiveTestPom() {'''
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
	    <sourceDirectory>/home/user/projects/te/rcp/org.testeditor.rcp4.uatests/src/main/java</sourceDirectory>
	    <scriptSourceDirectory>/home/user/projects/te/rcp/org.testeditor.rcp4.uatests/src/main/scripts</scriptSourceDirectory>
	    <testSourceDirectory>/home/user/projects/te/rcp/org.testeditor.rcp4.uatests/src/test/java</testSourceDirectory>
	    <outputDirectory>/home/user/projects/te/rcp/org.testeditor.rcp4.uatests/target/classes</outputDirectory>
	    <testOutputDirectory>/home/user/projects/te/rcp/org.testeditor.rcp4.uatests/target/test-classes</testOutputDirectory>
	    <resources>
	      <resource>
	        <directory>/home/user/projects/te/rcp/org.testeditor.rcp4.uatests/src/main/resources</directory>
	      </resource>
	    </resources>
	    <testResources>
	      <testResource>
	        <directory>/home/user/projects/te/rcp/org.testeditor.rcp4.uatests/src/test/resources</directory>
	      </testResource>
	    </testResources>
	    <directory>/home/user/projects/te/rcp/org.testeditor.rcp4.uatests/target</directory>
	    <finalName>org.testeditor.rcp4.uatests-1.2.0-SNAPSHOT</finalName>
	  </build>
	</project>
	'''	
	}
	
}