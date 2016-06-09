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
package org.testeditor.dsl.common.ui.utils

import static org.eclipse.xtext.xbase.lib.StringExtensions.isNullOrEmpty;

import java.io.File
import java.util.List
import javax.inject.Inject
import org.eclipse.core.resources.FileInfoMatcherDescription
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.IResourceFilterDescription
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.m2e.core.MavenPlugin
import org.eclipse.m2e.core.project.ResolverConfiguration
import org.eclipse.xtext.ui.XtextProjectHelper
import org.eclipse.xtext.util.StringInputStream
import org.osgi.framework.FrameworkUtil
import org.slf4j.LoggerFactory
import org.testeditor.dsl.common.ide.util.FileUtils
import java.util.Properties

/**
 * Generator to generate content to a new test project.
 */
class ProjectContentGenerator {

	static public val String MAVEN = "Maven"
	static public val String GRADLE = "Gradle"
	static public val String WEBFIXTURE = "Web Fixture"
	static public val String SRC_FOLDER = 'src/main/java'
	static public val String SRC_TEST_FOLDER = 'src/test/java'

	// NOT API yet.
	static val SWINGFIXTURE = "Swing Fixture"

	private static val logger = LoggerFactory.getLogger(ProjectContentGenerator)

	@Inject FileLocatorService fileLocatorService
	@Inject extension ProjectUtils

	def void createProjectContent(IProject project, String[] fixtures, String buildsystem, boolean demo,
		IProgressMonitor monitor) throws CoreException{
		project => [
			val projectMain = SRC_FOLDER + "/" + name
			val projectTest = SRC_TEST_FOLDER + "/" + name
			var String amlContent = null
			var IFile initAml = null
			if (buildsystem == MAVEN) {
				initAml = getFile(projectMain + "/" + name + ".aml")
				setupMavenProject(fixtures, monitor)
			}
			if (buildsystem == GRADLE) {
				initAml = getFile(projectTest + "/" + name + ".aml")
				setupGradleProject(fixtures, monitor)
			}
			createOrGetDeepFolder(projectMain)
			createOrGetDeepFolder(projectTest)
			if (demo) {
				amlContent = getDemoAMLContent(fixtures, name)
				fixtures.forEach[createDemoTestCase(project, SRC_TEST_FOLDER, monitor)]
			} else {
				amlContent = getInitialAMLContent(fixtures, name)
			}
			initAml.create(new StringInputStream(amlContent), IResource.NONE, monitor)
			if (buildsystem == GRADLE) {
				setupEclipseMetaData(monitor)
			}
			filterTechnicalProjectFiles(monitor)
		]
	}

	private def filterTechnicalProjectFiles(IProject project, IProgressMonitor monitor) {
		// make sure that no target folder is included into any resource set
		#["target", "build", "src-gen", "xtend-gen"].forEach [
			project.createFilter(IResourceFilterDescription.EXCLUDE_ALL.bitwiseOr(IResourceFilterDescription.FOLDERS),
				new FileInfoMatcherDescription("org.eclipse.core.resources.regexFilterMatcher", it), // hide (maven,gradle,xtend,xtext) generated/copied artifacts
				IResource.BACKGROUND_REFRESH, monitor)
		]
	}

	protected def void setupEclipseMetaData(IProject project, IProgressMonitor monitor) {
		val fileExtension = if (System.getProperty("os.name").toLowerCase.startsWith("win")) {
				".bat"
			} else {
				""
			}
		val completedFileName = project.location.toOSString + File.separator + "gradlew" + fileExtension
		if (!new File(completedFileName).canExecute) {
			logger.warn("completedFileName='{}' is not executable for user='{}'", completedFileName,
				System.getProperty("user.name"))
		}
		val command = #[completedFileName, "eclipse"]
		val processBuilder = new ProcessBuilder => [
			inheritIO
			redirectErrorStream(true)
			command(command)
			directory(project.location.toFile)
		]
		logger.info("Create eclipse project with gradle command={}", command)

		try {
			val result = processBuilder.start.waitFor
			if (result != 0) {
				logger.warn("gradle command returned with result='{}'", result)
			}
		} catch (Exception e) {
			logger.error("error during gradle command execution", e)
		}
		logger.debug("Project {} refreshed", project)
		project.refreshLocal(IProject.DEPTH_INFINITE, monitor)
	}

	protected def void setupGradleProject(IProject project, String[] fixtures, IProgressMonitor monitor) {
		var IFile buildFile = project.getFile("build.gradle")
		createGradleSettings(project, monitor);
		buildFile.create(new StringInputStream(getBuildGradleContent(fixtures)), IResource.NONE, monitor)
		val name = FrameworkUtil.getBundle(ProjectContentGenerator).symbolicName
		val bundleLocation = fileLocatorService.findBundleFileLocationAsString(name)
		val dest = project.location.toFile
		logger.info("using bundleLocation='{}' to copy gradlewrapper", bundleLocation)
		if (bundleLocation.endsWith(".jar")) {
			FileUtils.unpackZipFile(new File(bundleLocation), dest, "gradlewrapper/")
		} else {
			val src = new File(bundleLocation, "gradlewrapper")
			FileUtils.copyFolder(src, dest)
		}
		#["gradlew", "gradlew.bat"].forEach [
			val success = new File(dest, it).setExecutable(true, false)
			if (!success) {
				logger.warn("could not make file='{}' in dest='{}' executable", it, dest)
			}
		]
	}

	def createGradleSettings(IProject project, IProgressMonitor monitor) {
		var Properties props = System.getProperties();
		if (!isNullOrEmpty(props.getProperty("http.proxyHost"))) {
			var IFile gradleProperties = project.getFile("gradle.properties")
			gradleProperties.create(new StringInputStream(getProxyProperties()), IResource.NONE, monitor)
		}
	}

	def String getProxyProperties() {
		'''
		systemProp.http.proxyHost=«System.getProperties().getProperty("http.proxyHost")»
		systemProp.http.proxyPort=«System.getProperties().getProperty("http.proxyPort")»
		systemProp.http.proxyUser=«System.getProperties().getProperty("http.proxyUser")»
		systemProp.http.proxyPassword=«System.getProperties().getProperty("http.proxyPassword")»
		systemProp.https.proxyHost=«System.getProperties().getProperty("https.proxyHost")»
		systemProp.https.proxyPort=«System.getProperties().getProperty("https.proxyPort")»
		'''
	}

	protected def void setupMavenProject(IProject project, String[] fixtures, IProgressMonitor monitor) {
		var IFile buildFile = project.getFile("pom.xml")
		buildFile.create(new StringInputStream(getPomContent(fixtures, project.name)), IResource.NONE, monitor)
		val mavenSettings = System.getProperty("TE.MAVENSETTINGSPATH")
		if (!mavenSettings.isNullOrEmpty) {
			MavenPlugin.mavenConfiguration.userSettingsFile = mavenSettings
		}
		var configurationManager = MavenPlugin.projectConfigurationManager
		var configuration = new ResolverConfiguration
		configuration.resolveWorkspaceProjects = true
		configuration.selectedProfiles = ""
		project.addNature(XtextProjectHelper.NATURE_ID)
		configurationManager.enableMavenNature(project, configuration, monitor)
	}

	protected def void createDemoTestCase(String fixture, IProject project, String srcFolder,
		IProgressMonitor monitor) {
		if (fixture == WEBFIXTURE) {
			val tclFile = project.getFile(srcFolder + "/" + project.name + "/GoogleTest.tcl")
			tclFile.create(new StringInputStream(getGoogleTestCase(project.name)), false, monitor)
		}
	}

	def String getGoogleTestCase(String packageName) {
		'''
		package «packageName»
		
		import org.testeditor.fixture.web.*
		
		#GoogleTest 
		
		* Start Brwoser with google search engine.
		Component: WebBrowser
		- start browser <Firefox>
		- Browse to "http://www.google.de"
		
		* Search with google the Testeditor
		Component: Searchsite
		- Type in <Searchfield> value "testeditor"
		- press enter in <Searchfield> 
		
		* Close Browser
		Component: WebBrowser
		- Wait "3" seconds
		- Close browser		'''
	}

	def String getDemoAMLContent(String[] fixtures, String packageName) {
		'''
			package «packageName»
			
			«FOR fixture : fixtures»
				import «getPackage(fixture)»
			«ENDFOR»
			«FOR fixture : fixtures»
				«getDemoAMLComponentsContent(fixture)»
			«ENDFOR»		
		'''
	}

	def String getDemoAMLComponentsContent(String fixture) {
		if (fixture == WEBFIXTURE) {
			return '''
				component Searchsite is Page {
					element Searchfield is field {
						label = "Search field"
						locator ="q"
						
					}
				}
			'''

		}
		return ""
	}

	def String getInitialAMLContent(String[] fixtures, String packageName) {
		'''
			package «packageName»
			
			«FOR fixture : fixtures»
				import «getPackage(fixture)»
			«ENDFOR»
		'''
	}

	def String getPackage(String fixtureName) {
		if (fixtureName == WEBFIXTURE) {
			return "org.testeditor.fixture.web.*"
		}
		if (fixtureName == SWINGFIXTURE) {
			return "org.testeditor.fixture.swing.*"
		}
		return ""
	}

	def String getBuildGradleContent(String[] fixtureNames) {
		'''
			plugins {
			    id 'org.testeditor.gradle-plugin' version '0.1'
			    id 'maven'
			    id 'eclipse'
			}
			
			group = 'org.testeditor.demo'
			version = '1.0.0-SNAPSHOT'
			
			// In this section you declare where to find the dependencies of your project
			repositories {
			    jcenter()
			    maven { url "http://dl.bintray.com/test-editor/Fixtures" }
			    maven { url "http://dl.bintray.com/test-editor/test-dsls" }
			}
			
			// Configure the testeditor plugin
			testeditor {
				version '1.0.0'
			}
			
			// In this section you declare the dependencies for your production and test code
			dependencies {
			    «FOR s : fixtureNames»
			    	«getGradleDependency(s)»
				«ENDFOR»
			    testCompile 'junit:junit:4.12'
			}
		'''
	}

	def String getGradleDependency(String fixtureName) {
		if (fixtureName == WEBFIXTURE) {
			return '''
				compile 'org.testeditor.fixture:web-fixture:3.0.0-PROTO'
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
					<java.version>1.7</java.version>
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
							          <groupId>org.eclipse.m2e</groupId>
							          <artifactId>lifecycle-mapping</artifactId>
							          <version>1.0.0</version>
							          <configuration>
							              <lifecycleMappingMetadata>
							                    <pluginExecutions>
							                      <pluginExecution>
							                        <pluginExecutionFilter>
							                          <groupId>org.codehaus.mojo</groupId>
							                          <artifactId>build-helper-maven-plugin</artifactId>
							                          <versionRange>[1.0,)</versionRange>
							                          <goals>
							                            <goal>parse-version</goal>
							                            <goal>add-source</goal>
							                            <goal>maven-version</goal>
							                            <goal>add-resource</goal>
							                            <goal>add-test-resource</goal>
							                            <goal>add-test-source</goal>
							                          </goals>
							                        </pluginExecutionFilter>
							                        <action>
							                          <execute>
							                            <runOnConfiguration>true</runOnConfiguration>
							                            <runOnIncremental>true</runOnIncremental>
							                          </execute>
							                        </action>
							                      </pluginExecution>
							                  </pluginExecutions>
							              </lifecycleMappingMetadata>
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
								<sourceRoots>
									<sourceRoot>«SRC_TEST_FOLDER»</sourceRoot>
									<sourceRoot>«SRC_FOLDER»</sourceRoot>
								</sourceRoots>
									<languages>
										<language>
											<setup>org.testeditor.tsl.dsl.TslStandaloneSetup</setup>
										</language>
										<language>
											<setup>org.testeditor.tcl.dsl.TclStandaloneSetup</setup>
											<outputConfigurations>
												<outputConfiguration>
													<outputDirectory>src_gen/test/java</outputDirectory>
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
						              <artifactId>org.testeditor.dsl.common</artifactId>
						              <version>${project.version}</version>
						            </dependency>
						            <dependency>
										<groupId>org.testeditor</groupId>
										<artifactId>org.testeditor.tml.model</artifactId>
										<version>${project.version}</version>
									</dependency>
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
										<artifactId>org.testeditor.tml.dsl</artifactId>
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
						<plugin>
						    <groupId>org.codehaus.mojo</groupId>
						    <artifactId>build-helper-maven-plugin</artifactId>
						    <version>1.7</version>
						    <executions>
						        <execution>
						            <id>add-test-source</id>
						            <phase>generate-test-sources</phase>
						            <goals>
						                <goal>add-test-source</goal>
						            </goals>
						            <configuration>
						                <sources>
						                    <source>src_gen/test/java</source>
						                </sources>
						            </configuration>
						        </execution>
						    </executions>
						</plugin>
					</plugins>
				</build>
			
			</project>
		'''
	}

	def String getMavenDependency(String fixtureName) {
		if (fixtureName == WEBFIXTURE) {
			return '''
				<dependency>
					<groupId>org.testeditor.fixture</groupId>
					<artifactId>web-fixture</artifactId>
					<version>3.0.0-PROTO</version>
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

	def List<String> getAvailableBuildSystems() {
		return #[GRADLE, MAVEN]
	}

	def List<String> getAvailableFixtureNames() {
		return #[WEBFIXTURE, SWINGFIXTURE]
	}

	private def void addNature(IProject newProject, String nature) {
		if (!newProject.hasNature(nature)) {
			val description = newProject.getDescription
			description.setNatureIds(description.getNatureIds + #[nature])
			newProject.setDescription(description, null)
		}
	}

}
