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
package org.testeditor.dsl.common.ide.util

import java.io.ByteArrayInputStream
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.m2e.core.MavenPlugin
import org.eclipse.m2e.core.project.ResolverConfiguration

/**
 * Generator to genrate content to a new test project
 */
class ProjectContentGenerator {

	static val MAVEN = "Maven"
	static val GRADLE = "Gradle"
	static val WEBFIXTURE = "Web Fixture"
	static val SWINGFIXTURE = "Swing Fixture"

	def createProjectContent(IProject project, String[] fixtures, String buildsystem,
		boolean demo) throws CoreException{
		if (buildsystem == MAVEN) {
			var IFile buildFile = project.getFile("pom.xml")
			buildFile.create(new ByteArrayInputStream(getPomContent(fixtures, project.name).getBytes()), IResource.NONE,
				new NullProgressMonitor())
			var configurationManager = MavenPlugin.getProjectConfigurationManager()
			var configuration = new ResolverConfiguration();
			configuration.setResolveWorkspaceProjects(true);
			configuration.setSelectedProfiles("");
			configurationManager.enableMavenNature(project, configuration, new NullProgressMonitor())
			configurationManager.updateProjectConfiguration(project, new NullProgressMonitor())
		}
		if (buildsystem == GRADLE) {
			var buildFile = project.getFile("build.gradle")
			buildFile.create(new ByteArrayInputStream(getBuildGradleContent(fixtures).getBytes()), IResource.NONE,
				new NullProgressMonitor())
		}
		project.getFolder("src/main/java/" + project.name).create(true, false, new NullProgressMonitor())
		var initAml = project.getFile("src/main/java/" + project.name + "/" + project.name + ".aml")
		initAml.create(new ByteArrayInputStream(getInitialAMLContent(fixtures, project.name).getBytes()),
			IResource.NONE, new NullProgressMonitor())
	}

	def String getInitialAMLContent(String[] fixtures, String packageName) {
		'''
			package «packageName»
			
			«FOR fixture : fixtures»
				import «getPackage(fixture)»
			«ENDFOR»
		'''
	}

	def getPackage(String fixtureName) {
		if (fixtureName == WEBFIXTURE) {
			return "org.testeditor.fixture.web.*"
		}
		if (fixtureName == WEBFIXTURE) {
			return "org.testeditor.fixture.swing.*"
		}
		return ""
	}

	def String getBuildGradleContent(String[] fixtureNames) {
		'''
			plugins {
			    id 'org.testeditor.gradle-plugin' version '0.1'
			}
			
			
			// Apply the java plugin to add support for Java
			apply plugin: 'java'
			
			// In this section you declare where to find the dependencies of your project
			repositories {
			    jcenter()
			}
			
			// In this section you declare the dependencies for your production and test code
			dependencies {
			    compile 'org.slf4j:slf4j-api:latest.release'
			    «FOR s : fixtureNames»
			    	«getGradleDependency(s)»
				«ENDFOR»
			    testCompile 'junit:junit:latest.release'
			}
		'''
	}

	def getGradleDependency(String fixtureName) {
		if (fixtureName == WEBFIXTURE) {
			return '''
				compile 'org.testeditor.fixture:web-fixture:3.0.0-SNAPSHOT'
			'''
		}
		if (fixtureName == SWINGFIXTURE) {
			return '''
				compile 'org.testeditor.fixture:swing-fixture:3.0.0-PROTO'
			'''
		}
	}

	def String getPomContent(String[] fixtureNames, String projectName) {
		'''
			<?xml version="1.0" encoding="UTF-8"?>
			<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
				xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
				<modelVersion>4.0.0</modelVersion>
			
				<groupId>org.testeditor.project</groupId>
				<artifactId>«projectName»</artifactId>
				<version>1.0.0-SNAPSHOT</version>
			
				<properties>
					<!-- Version definitions below -->
					<java.version>1.8</java.version>
					<project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
			
					<maven-clean-plugin.version>2.5</maven-clean-plugin.version>
					<maven-resources-plugin.version>2.7</maven-resources-plugin.version>
					<maven-compiler-plugin.version>3.3</maven-compiler-plugin.version>
			
					<xtext.version>2.9.0</xtext.version>
					<xtend.version>${xtext.version}</xtend.version>
				</properties>
			
				<repositories>
					<repository>
						<snapshots>
							<enabled>false</enabled>
						</snapshots>
						<id>central</id>
						<name>bintray</name>
						<url>http://jcenter.bintray.com</url>
					</repository>
					<repository>
						<snapshots>
							<enabled>false</enabled>
						</snapshots>
						<id>bintray-test-editor-Fixtures</id>
						<name>bintray</name>
						<url>http://dl.bintray.com/test-editor/Fixtures</url>
					</repository>
				</repositories>
			
				<dependencies>
					<dependency>
						<groupId>org.eclipse.xtend</groupId>
						<artifactId>org.eclipse.xtend.lib</artifactId>
						<version>${xtend.version}</version>
					</dependency>
					<dependency>
						<groupId>junit</groupId>
						<artifactId>junit</artifactId>
						<version>4.12</version>
					</dependency>
					«FOR s : fixtureNames»
						«getMavenDependency(s)»
					«ENDFOR»
				</dependencies>
			
				<build>
					<pluginManagement>
						<plugins>
							<plugin>
								<groupId>org.apache.maven.plugins</groupId>
								<artifactId>maven-resources-plugin</artifactId>
								<version>${maven-resources-plugin.version}</version>
								<configuration>
									<encoding>${project.build.sourceEncoding}</encoding>
								</configuration>
							</plugin>
							<plugin>
								<groupId>org.apache.maven.plugins</groupId>
								<artifactId>maven-compiler-plugin</artifactId>
								<version>${maven-compiler-plugin.version}</version>
								<configuration>
									<source>${java.version}</source>
									<target>${java.version}</target>
								</configuration>
							</plugin>
							<plugin>
								<groupId>org.eclipse.xtext</groupId>
								<artifactId>xtext-maven-plugin</artifactId>
								<version>${xtext.version}</version>
								<executions>
									<execution>
										<goals>
											<goal>generate</goal>
										</goals>
									</execution>
								</executions>
								<configuration>
									<languages>
										<language>
											<setup>org.testeditor.tsl.dsl.TslStandaloneSetup</setup>
										</language>
										<language>
											<setup>org.testeditor.tcl.dsl.TclStandaloneSetup</setup>
											<outputConfigurations>
												<outputConfiguration>
													<outputDirectory>src/test/java</outputDirectory>
												</outputConfiguration>
											</outputConfigurations>
										</language>
										<language>
											<setup>org.testeditor.aml.dsl.AmlStandaloneSetup</setup>
										</language>
									</languages>
								</configuration>
								<dependencies>
									<dependency>
										<groupId>org.testeditor</groupId>
										<artifactId>org.testeditor.tsl.model</artifactId>
										<version>${project.version}</version>
									</dependency>
									<dependency>
										<groupId>org.testeditor</groupId>
										<artifactId>org.testeditor.tsl.dsl</artifactId>
										<version>${project.version}</version>
									</dependency>
									<dependency>
										<groupId>org.testeditor</groupId>
										<artifactId>org.testeditor.tcl.model</artifactId>
										<version>${project.version}</version>
									</dependency>
									<dependency>
										<groupId>org.testeditor</groupId>
										<artifactId>org.testeditor.tcl.dsl</artifactId>
										<version>${project.version}</version>
									</dependency>
									<dependency>
										<groupId>org.testeditor</groupId>
										<artifactId>org.testeditor.aml.model</artifactId>
										<version>${project.version}</version>
									</dependency>
									<dependency>
										<groupId>org.testeditor</groupId>
										<artifactId>org.testeditor.aml.dsl</artifactId>
										<version>${project.version}</version>
									</dependency>
								</dependencies>
							</plugin>
							<plugin>
								<groupId>org.eclipse.xtend</groupId>
								<artifactId>xtend-maven-plugin</artifactId>
								<version>${xtend.version}</version>
							</plugin>
						</plugins>
					</pluginManagement>
					<plugins>
						<plugin>
							<groupId>org.eclipse.xtext</groupId>
							<artifactId>xtext-maven-plugin</artifactId>
						</plugin>
						<plugin>
							<groupId>org.eclipse.xtend</groupId>
							<artifactId>xtend-maven-plugin</artifactId>
						</plugin>
					</plugins>
				</build>
			
			</project>
		'''
	}

	def getMavenDependency(String fixtureName) {
		if (fixtureName == WEBFIXTURE) {
			return '''
				<dependency>
					<groupId>org.testeditor.fixture</groupId>
					<artifactId>web-fixture</artifactId>
					<version>3.0.0-SNAPSHOT</version>
				</dependency>
			'''
		}
		if (fixtureName == SWINGFIXTURE) {
			return '''
				<dependency>
					<groupId>org.testeditor.fixture</groupId>
					<artifactId>swing-fixture</artifactId>
					<version>3.0.0-PROTO</version>
				</dependency>
			'''
		}
	}

	def getAvailableBuildSystems() {
		return #[MAVEN, GRADLE]
	}

	def getAvailableFixtureNames() {
		return #[WEBFIXTURE, SWINGFIXTURE]
	}

}
