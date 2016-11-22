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

import com.google.common.annotations.VisibleForTesting
import java.io.File
import java.util.List
import java.util.Properties
import javax.inject.Inject
import org.eclipse.core.resources.FileInfoMatcherDescription
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.IResourceFilterDescription
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.Path
import org.eclipse.jdt.core.JavaCore
import org.eclipse.m2e.core.MavenPlugin
import org.eclipse.m2e.core.project.ResolverConfiguration
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.xtext.ui.XtextProjectHelper
import org.eclipse.xtext.util.StringInputStream
import org.osgi.framework.FrameworkUtil
import org.slf4j.LoggerFactory
import org.testeditor.dsl.common.ide.util.FileUtils
import org.testeditor.dsl.common.ui.wizards.SwingDemoContentGenerator
import org.testeditor.dsl.common.util.GradleHelper
import org.testeditor.dsl.common.util.classpath.ClasspathUtil

import static org.eclipse.xtext.xbase.lib.StringExtensions.isNullOrEmpty

/**
 * Generator to generate content to a new test project.
 */
class ProjectContentGenerator {

	static public val TEST_EDITOR_VERSION = "1.1.0" // TODO this sucks - extract to VersionHelper and use the newest version
	
	static public val String TEST_EDITOR_MVN_GEN_OUTPUT = 'src-gen/test/java'

	static public val String MAVEN = "Maven"
	static public val String GRADLE = "Gradle"
	static public val String WEBFIXTURE = "Web Fixture"
	static public val String SWINGFIXTURE = "Swing Fixture"
	static public val String SRC_FOLDER = 'src/main/java'
	static public val String SRC_TEST_FOLDER = 'src/test/java'

	private static val logger = LoggerFactory.getLogger(ProjectContentGenerator)

	@Inject FileLocatorService fileLocatorService
	@Inject extension ProjectUtils
	@Inject extension ClasspathUtil
	@Inject GradleHelper gradleHelper
	@Inject SwingDemoContentGenerator swingDemoContentGenerator
	
	@Accessors(PUBLIC_GETTER) 
	IFile demoTclFile

	def void createProjectContent(IProject project, String[] fixtures, String buildsystem, boolean demo,
		IProgressMonitor monitor) throws CoreException{
		project => [
			// setup project structure
			val srcMain = SRC_FOLDER + "/" + name
			val srcTest = SRC_TEST_FOLDER + "/" + name
			if (buildsystem == MAVEN) {
				setupMavenProject(fixtures, monitor)
			}
			if (buildsystem == GRADLE) {
				setupGradleProject(fixtures, monitor)
			}
			createOrGetDeepFolder(srcMain)
			createOrGetDeepFolder(srcTest)
			
			// fill project with sample code
			if (demo) {
				fixtures.forEach[createApplicationCode(project, srcMain, monitor)]
				fixtures.forEach[createAmlCode(project, srcTest, monitor)]
				fixtures.forEach[createDemoTestCase(project, srcTest, monitor)]
			}
			
			// some more technical project setup
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

	@VisibleForTesting
	protected def void setupEclipseMetaData(IProject project, IProgressMonitor monitor) {
		// Run "gradle eclipse" to create the meta-data
		try {
			monitor.taskName = "Initializing Eclipse project."
			gradleHelper.runTasks(project, "eclipse")
		} catch (Exception e) {
			logger.error("Execution of 'gradle eclipse' failed with an exception.", e)
		}
    
		// Refresh the Eclipse project
		project.refreshLocal(IProject.DEPTH_INFINITE, monitor)
		logger.debug("Refreshed project='{}'.", project)
	}

	protected def void setupGradleProject(IProject project, String[] fixtures, IProgressMonitor monitor) {
		var IFile buildFile = project.getFile("build.gradle")
		createGradleSettings(project, monitor)
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

	private def void createGradleSettings(IProject project, IProgressMonitor monitor) {
		var Properties props = System.getProperties();
		if (!isNullOrEmpty(props.getProperty("http.proxyHost"))) {
			var IFile gradleProperties = project.getFile("gradle.properties")
			gradleProperties.create(new StringInputStream(getProxyProperties()), IResource.NONE, monitor)
		}
	}

	def String getProxyProperties()  '''
		systemProp.http.proxyHost=«System.properties.getProperty("http.proxyHost")»
		systemProp.http.proxyPort=«System.properties.getProperty("http.proxyPort")»
		systemProp.http.proxyUser=«System.properties.getProperty("http.proxyUser")»
		systemProp.http.proxyPassword=«System.properties.getProperty("http.proxyPassword")»
		systemProp.https.proxyHost=«System.properties.getProperty("https.proxyHost")»
		systemProp.https.proxyPort=«System.properties.getProperty("https.proxyPort")»
	'''

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
		project.setupMavenTclGeneratorPreferences
		project.setupClasspathFileExclusions
	}

	// Eclipse build tries to copy files that are possibly already existent in the target folder. 
	// Excluding these from the copy job keeps stdout somewhat cleaner, removing all that 
	// noisy exceptions that report this mismatch
	private def void setupClasspathFileExclusions(IProject project) {
		val fileExclusions = #["aml", "tcl", "tsl", "config", "tml", "_trace"].map[new Path('''**/*.«it»''')]

		project.transformClasspathEntries [
			if (path.segments.exists[matches("src(-gen)?")]) {
				return JavaCore.newSourceEntry(path, fileExclusions) // don't copy these, will reduce exceptions
			} else {
				return it
			}
		]
	}

	private def void setupMavenTclGeneratorPreferences(IProject project) {
		val tclPrefs = instanceScope.getPrefsNode('org.testeditor.tcl.dsl.Tcl')
		tclPrefs => [
			put('outlet.DEFAULT_OUTPUT.directory', './' + TEST_EDITOR_MVN_GEN_OUTPUT)
			putBoolean('BuilderConfiguration.is_project_specific', true)
			save
		]
	}

	private def void createApplicationCode(String fixture, IProject project, String srcFolder, IProgressMonitor monitor) {
		if (fixture == SWINGFIXTURE) {
			val greetingApplication = swingDemoContentGenerator.getSwingApplicationCode(project.name)
			greetingApplication.write(project, srcFolder, "GreetingApplication.java", monitor)
		}
	}

	private def void createAmlCode(String fixture, IProject project, String srcFolder, IProgressMonitor monitor) {
		if (fixture == WEBFIXTURE) {
			val google = '''
				«getInitialFileContents(project.name, fixture)»
				
				/**
				 * Application model for the google search site. It contains only the search field and
				 * binds it to the test-editor web fixture field element.
				 */
				component Searchsite is Page {
					element Searchfield is field {
						label = "Search field"
						locator = "q"
					}
				}
			'''
			google.write(project, srcFolder, "Google.aml", monitor)
		} else if (fixture == SWINGFIXTURE) {
			// Write GreetingApplication.aml
			val greetingApplication = '''
				«getInitialFileContents(project.name, fixture)»
				
				«swingDemoContentGenerator.amlContents»
			'''
			greetingApplication.write(project, srcFolder, "GreetingApplication.aml", monitor)
			
			// Write Swing.aml
			val swing = swingDemoContentGenerator.getSwingAmlContents(project.name)
			swing.write(project, srcFolder, "Swing.aml", monitor)
		}
	}

	private def void createDemoTestCase(String fixture, IProject project, String srcFolder, IProgressMonitor monitor) {
		if (fixture == WEBFIXTURE) {
			val googleTest = '''
				«getInitialFileContents(project.name, fixture)»

				«googleTestCase»
			'''
			demoTclFile = googleTest.write(project, srcFolder, "GoogleTest.tcl",  monitor)
		} else if (fixture == SWINGFIXTURE) {
			val greetingTest = '''
				«getInitialFileContents(project.name, fixture)»
				
				«swingDemoContentGenerator.getTestCase(project.name)»
			'''
			demoTclFile = greetingTest.write(project, srcFolder, "GreetingTest.tcl", monitor)
		}
	}

	private def IFile write(CharSequence contents, IProject project, String srcFolder, String fileName, IProgressMonitor monitor) {
		val file = project.getFile('''«srcFolder»/«fileName»''')
		file.create(new StringInputStream(contents.toString), false, monitor)
		return file
	}

	def String getGoogleTestCase() '''
		/**
		* This is a  demo test case. It is executable and uses the test-editor webfixture to automate a Google search.
		* The Google search mask is modelled in an AML file. You can navigate to the aml definitions of fixture code,
		* by ctrl + mouse click or F3 on an element.
		*
		* For example:
		* - access the google.aml by F3 on 'Component: Searchsite'
		* - access the webfixture aml by F3 on 'Component: WebBrowser'
		*/
		
		# GoogleTest
		
		* Start browser and navigate to Google
		
			Component: WebBrowser
			- Start <Firefox>
			- Browse to "http://www.google.de"
		
		* Search ^for "testeditor"
		
			Component: Searchsite
			- Type in <Searchfield> value "testeditor"
			- Press enter on <Searchfield>
		
		* Close browser
		
			Component: WebBrowser
			- Wait "3" seconds
			- Close browser
	'''

	def String getDemoAMLContent(String[] fixtures, String packageName) '''
		package «packageName»

		«FOR fixture : fixtures»
			import «getPackage(fixture)»
		«ENDFOR»
		«FOR fixture : fixtures»
			«getDemoAMLComponentsContent(fixture)»
		«ENDFOR»
	'''

	def String getDemoAMLComponentsContent(String fixture) {
		if (fixture == WEBFIXTURE) {
			return '''
				/**
				 * Application model for the google search site. It contains only the search field and
				 * binds it to the test-editor web fixture field element.
				 */
				component Searchsite is Page {
					element Searchfield is field {
						label = "Search field"
						locator = "q"
					}
				}
			'''
		} else if (fixture == SWINGFIXTURE) {
			return swingDemoContentGenerator.amlContents
		}
		return ""
	}

	def String getInitialFileContents(String packageName, String... fixturesToImport) '''
		package «packageName»

		«FOR fixture : fixturesToImport»
			import «fixture.package».*
		«ENDFOR»
	'''

	@VisibleForTesting
	protected def String getPackage(String fixtureName) {
		if (fixtureName == WEBFIXTURE) {
			return "org.testeditor.fixture.web"
		}
		if (fixtureName == SWINGFIXTURE) {
			return "org.testeditor.fixture.swing"
		}
		throw new IllegalArgumentException("Unknown fixture with name: " + fixtureName)
	}

	def String getBuildGradleContent(String[] fixtureNames) {
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
				version '«TEST_EDITOR_VERSION»'
			}

			// In this section you declare the dependencies for your production and test code
			dependencies {
				compile 'org.testeditor.fixture:core-fixture:3.1.0'
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
				compile 'org.testeditor.fixture:web-fixture:3.1.0'
			'''
		}
		if (fixtureName == SWINGFIXTURE) {
			return '''
				compile 'org.testeditor.fixture:swing-fixture:3.1.0'
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

					<xtext.version>2.9.1</xtext.version>
					<xtend.version>${xtext.version}</xtend.version>

					<testeditor.version>«TEST_EDITOR_VERSION»</testeditor.version>
					<testeditor.output>«TEST_EDITOR_MVN_GEN_OUTPUT»</testeditor.output>
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
						<id>test-editor-Fixtures</id>
						<name>test-editor-Fixtures</name>
						<url>http://dl.bintray.com/test-editor/Fixtures</url>
					</repository>
					<repository>
						<snapshots>
							<enabled>false</enabled>
						</snapshots>
						<id>test-editor-maven</id>
						<name>test-editor-maven</name>
						<url>http://dl.bintray.com/test-editor/test-editor-maven</url>
					</repository>
				</repositories>
				<pluginRepositories>
					<pluginRepository>
						<snapshots>
							<enabled>false</enabled>
						</snapshots>
						<id>bintray-test-editor-maven</id>
						<name>bintray-plugins</name>
						<url>http://dl.bintray.com/test-editor/test-editor-maven</url>
					</pluginRepository>
				</pluginRepositories>

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
					<dependency>
						<groupId>org.testeditor.fixture</groupId>
						<artifactId>core-fixture</artifactId>
						<version>3.1.0</version>
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
								<groupId>org.apache.maven.plugins</groupId>
								<artifactId>maven-clean-plugin</artifactId>
								<version>${maven-clean-plugin.version}</version>
								<configuration>
									<filesets>
										<fileset>
											<directory>${testeditor.output}</directory>
											<includes>
												<include>**</include>
											</includes>
											<excludes>
												<exclude>.gitignore</exclude>
											</excludes>
										</fileset>
									</filesets>
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
											<setup>org.testeditor.tml.dsl.TmlStandaloneSetup</setup>
										</language>
										<language>
											<setup>org.testeditor.tcl.dsl.TclStandaloneSetup</setup>
											<outputConfigurations>
												<outputConfiguration>
													<outputDirectory>${testeditor.output}</outputDirectory>
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
										<groupId>org.apache.commons</groupId>
										<artifactId>commons-lang3</artifactId>
										<version>3.4</version>
									</dependency>
									<dependency>
										<groupId>org.testeditor</groupId>
										<artifactId>org.testeditor.dsl.common</artifactId>
										<version>${testeditor.version}</version>
									</dependency>
									<dependency>
										<groupId>org.testeditor</groupId>
										<artifactId>org.testeditor.tsl.model</artifactId>
										<version>${testeditor.version}</version>
									</dependency>
									<dependency>
										<groupId>org.testeditor</groupId>
										<artifactId>org.testeditor.tsl.dsl</artifactId>
										<version>${testeditor.version}</version>
									</dependency>
									<dependency>
										<groupId>org.testeditor</groupId>
										<artifactId>org.testeditor.tml.model</artifactId>
										<version>${testeditor.version}</version>
									</dependency>
									<dependency>
										<groupId>org.testeditor</groupId>
										<artifactId>org.testeditor.tml.dsl</artifactId>
										<version>${testeditor.version}</version>
									</dependency>
									<dependency>
										<groupId>org.testeditor</groupId>
										<artifactId>org.testeditor.tcl.model</artifactId>
										<version>${testeditor.version}</version>
									</dependency>
									<dependency>
										<groupId>org.testeditor</groupId>
										<artifactId>org.testeditor.tcl.dsl</artifactId>
										<version>${testeditor.version}</version>
									</dependency>
									<dependency>
										<groupId>org.testeditor</groupId>
										<artifactId>org.testeditor.aml.model</artifactId>
										<version>${testeditor.version}</version>
									</dependency>
									<dependency>
										<groupId>org.testeditor</groupId>
										<artifactId>org.testeditor.aml.dsl</artifactId>
										<version>${testeditor.version}</version>
									</dependency>
									<dependency>
										<groupId>org.gradle</groupId>
										<artifactId>gradle-tooling-api</artifactId>
										<version>2.14</version>
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
						                    <source>${testeditor.output}</source>
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
					<version>3.1.0</version>
				</dependency>
			'''
		}
		if (fixtureName == SWINGFIXTURE) {
			return '''
				<dependency>
					<groupId>org.testeditor.fixture</groupId>
					<artifactId>swing-fixture</artifactId>
					<version>3.1.0</version>
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
