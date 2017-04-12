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
import javax.inject.Inject
import org.eclipse.core.resources.FileInfoMatcherDescription
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.IResourceFilterDescription
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.Path
import org.eclipse.core.runtime.Platform
import org.eclipse.jdt.core.JavaCore
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.xtext.ui.XtextProjectHelper
import org.eclipse.xtext.util.StringInputStream
import org.osgi.framework.FrameworkUtil
import org.osgi.framework.Version
import org.slf4j.LoggerFactory
import org.testeditor.dsl.common.ide.util.FileUtils
import org.testeditor.dsl.common.ui.wizards.SwingDemoContentGenerator
import org.testeditor.dsl.common.util.GradleHelper
import org.testeditor.dsl.common.util.MavenExecutor
import org.testeditor.dsl.common.util.classpath.ClasspathUtil

/**
 * Generator to generate content to a new test project.
 */
class ProjectContentGenerator {

	static public val TEST_EDITOR_MAVEN_PLUGIN_VERSION = "1.1"
	static public val TEST_EDITOR_WEB_FIXTURE = "3.1.4"
	static public val TEST_EDITOR_CORE_FIXTURE = "3.1.0"
	static public val TEST_EDITOR_SWING_FIXTURE = "3.1.1"
	static public val TEST_EDITOR_GRADLE_PLUGIN_VERSION = "0.5"
	
	static public val String TEST_EDITOR_MVN_GEN_OUTPUT = 'src-gen/test/java'

	static public val String MAVEN = "Maven"
	static public val String GRADLE = "Gradle"
	static public val String WEBFIXTURE = "Web Fixture"
	static public val String SWINGFIXTURE = "Swing Fixture"
	static public val String SRC_FOLDER = 'src/main/java'
	static public val String RESOURCES_FOLDER = 'src/main/resources'
	static public val String SRC_TEST_FOLDER = 'src/test/java'
	static public val String RESOURCES_TEST_FOLDER = 'src/test/resources'

	private static val logger = LoggerFactory.getLogger(ProjectContentGenerator)

	val String testEditorVersion // is filled by querying the plugin version

	@Inject FileLocatorService fileLocatorService
	@Inject MavenExecutor mavenExecutor
	@Inject GradleHelper gradleHelper
	@Inject SwingDemoContentGenerator swingDemoContentGenerator
	@Inject extension ProjectUtils
	@Inject extension ClasspathUtil
	
	@Accessors(PUBLIC_GETTER) 
	IFile demoTclFile

	def List<String> getAvailableBuildSystems() {
		return #[GRADLE, MAVEN]
	}

	def List<String> getAvailableFixtureNames() {
		return #[WEBFIXTURE, SWINGFIXTURE]
	}
	
	new() {
		testEditorVersion = bundleVersion.mapTesteditorVersion
	}

	@VisibleForTesting
	protected def Version getBundleVersion() {
		val bundle = Platform.getBundle("org.testeditor.dsl.common.ui")
		return bundle?.version ?: Version.valueOf("0.0.0")
	}
	
	@VisibleForTesting
	protected def String mapTesteditorVersion(Version version){
		return '''«version.major».«version.minor».«version.micro»«if(!version.qualifier.nullOrEmpty){'-SNAPSHOT'}else{''}»'''
	}
	
	@VisibleForTesting
	protected def String getXtextVersion() {
		val xtextVersion = Platform.getBundle("org.eclipse.xtext").version
		return '''«xtextVersion.major».«xtextVersion.minor».«xtextVersion.micro»'''
	}

	def void createProjectContent(IProject project, String[] fixtures, String buildsystem, boolean demo,
		IProgressMonitor monitor) throws CoreException{
		project => [
			// setup folder structure
			createSourceFolder
			createLoggingConfig(monitor)
			
			// setup buildsystem
			if (buildsystem == MAVEN) {
				setupMavenProject(fixtures, monitor)
			}
			if (buildsystem == GRADLE) {
				setupGradleProject(fixtures, monitor)
			}
			
			// fill project with sample code
			if (demo) {
				val mainSourceRoot = SRC_FOLDER + "/" + name
				val testSourceRoot = SRC_TEST_FOLDER + "/" + name
				fixtures.forEach[createApplicationCode(project, mainSourceRoot, monitor)]
				fixtures.forEach[createAmlCode(project, testSourceRoot, monitor)]
				fixtures.forEach[createDemoTestCase(project, testSourceRoot, monitor)]
			}
			
			// some more technical project setup
			if (buildsystem == MAVEN) {
				setupMavenEclipseMetaData(monitor)
				setupMavenSourceClassPaths
				setupClasspathFileExclusions
			}
			if (buildsystem == GRADLE) {
				setupEclipseMetaData(monitor)
				setupGradleSourceClassPaths
			}
			
			// TE-470 project file filter to allow access to src etc. are not activated
			// filterTechnicalProjectFiles(monitor)
		]
	}

	private def void createSourceFolder(IProject project) {
		createOrGetDeepFolder(project, SRC_FOLDER)
		createOrGetDeepFolder(project, RESOURCES_FOLDER)
		createOrGetDeepFolder(project, SRC_TEST_FOLDER)
		createOrGetDeepFolder(project, RESOURCES_TEST_FOLDER)
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

	private def void setupGradleProject(IProject project, String[] fixtures, IProgressMonitor monitor) {
		createGradleBuildFile(project, fixtures, monitor)
		maybeCreateGradleSettings(project, monitor)
		createGradleWrapper(project)
	}

	private def void setupGradleSourceClassPaths(IProject project) {
		val wantedSourceClassPaths = #[ //
			SRC_FOLDER,
			RESOURCES_FOLDER,
			SRC_TEST_FOLDER,
			RESOURCES_TEST_FOLDER,
			"build/tcl", // generated test cases
			"build/tclConfig", // generated test configurations
			"build/tclMacro" // generated test macros
		]
		project.setupSourceClassPaths(wantedSourceClassPaths)
	}
	
	private def void setupMavenSourceClassPaths(IProject project) {
		val wantedSourceClassPaths = #[ //
			SRC_FOLDER,
			RESOURCES_FOLDER,
			SRC_TEST_FOLDER,
			RESOURCES_TEST_FOLDER
		]
		project.setupSourceClassPaths(wantedSourceClassPaths)
	}

	private def void setupSourceClassPaths(IProject project, Iterable<String> paths) {
		val classPathPrefix = '''/«project.name»/'''
		val existingSourcePaths = project.sourceClasspathEntries.map[path.toPortableString]

		val pathsToAdd = paths.filter [ wantedPath |
			!existingSourcePaths.exists[endsWith(wantedPath)]
		]

		val classPathEntriesToAdd = pathsToAdd.map[JavaCore.newSourceEntry(Path.fromPortableString('''«classPathPrefix»«it»'''))]
		project.addClasspathEntries(classPathEntriesToAdd)
	}

	private def void createGradleBuildFile(IProject project, String[] fixtures, IProgressMonitor monitor) {
		val buildFile = project.getFile("build.gradle")
		val contents = getBuildGradleContent(fixtures)
		buildFile.create(new StringInputStream(contents), IResource.NONE, monitor)
	}

	private def void maybeCreateGradleSettings(IProject project, IProgressMonitor monitor) {
		val isHttpProxySet = !System.getProperty("http.proxyHost").nullOrEmpty
		if (isHttpProxySet) {
			val gradleProperties = project.getFile("gradle.properties")
			gradleProperties.create(new StringInputStream(proxyProperties), IResource.NONE, monitor)
		}
	}

	private def String getProxyProperties()  '''
		systemProp.http.proxyHost=«System.properties.getProperty("http.proxyHost")»
		systemProp.http.proxyPort=«System.properties.getProperty("http.proxyPort")»
		systemProp.http.proxyUser=«System.properties.getProperty("http.proxyUser")»
		systemProp.http.proxyPassword=«System.properties.getProperty("http.proxyPassword")»
		systemProp.https.proxyHost=«System.properties.getProperty("https.proxyHost")»
		systemProp.https.proxyPort=«System.properties.getProperty("https.proxyPort")»
		systemProp.http.nonProxyHosts=«System.properties.getProperty("https.nonProxyHosts")»
	'''
	
	private def void createGradleWrapper(IProject project) {
		val name = FrameworkUtil.getBundle(ProjectContentGenerator).symbolicName
		val bundleLocation = fileLocatorService.findBundleFileLocationAsString(name)
		logger.info("using bundleLocation='{}' to copy gradlewrapper", bundleLocation)
		val dest = project.location.toFile
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
	
	private def IFile getMavenBuildFile(IProject project) {
		return project.getFile("pom.xml")
	}
	
	protected def void setupMavenProject(IProject project, String[] fixtures, IProgressMonitor monitor) {
		val projectContentStream = new StringInputStream(getPomContent(fixtures, project.name))
		project.mavenBuildFile.create(projectContentStream, IResource.NONE, monitor)
	}
		
	private def void setupMavenEclipseMetaData(IProject project, IProgressMonitor monitor) {
		val pathToMavenBuildFile = project.mavenBuildFile.location.removeLastSegments(1).toOSString
		mavenExecutor.executeInNewJvm("eclipse:eclipse", pathToMavenBuildFile, null, monitor, System.out)
		project => [
			refreshLocal(IResource.DEPTH_INFINITE, monitor)
			addNature(JavaCore.NATURE_ID)
			addNature(XtextProjectHelper.NATURE_ID)
			setupMavenTclGeneratorPreferences
		]
	}

	/**
	 * Add file exclusions to classpaths to avoid copy errors during eclipse build steps.<br/><br/>
	 * 
	 * Eclipse build tries to copy files that are possibly already existent in the target folder. 
	 * Excluding these from the copy job keeps stdout somewhat cleaner, removing all that 
	 * noisy exceptions that report this mismatch.
	 */	
	private def void setupClasspathFileExclusions(IProject project) {
		val fileExtensions = #["aml", "tcl", "tsl", "config", "tml", "_trace"]
		val fileExclusions = fileExtensions.map[new Path('''**/*.«it»''')]

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
			greetingApplication.write(project, srcFolder, "SwingGreetingApplication.java", monitor)
		}
	}

	private def void createAmlCode(String fixture, IProject project, String srcFolder, IProgressMonitor monitor) {
		if (fixture == WEBFIXTURE) {
			val google = '''
				«getInitialFileContents(project.name, fixture)»
				import static org.testeditor.fixture.web.LocatorStrategy.NAME
				
				/**
				 * Application model for the google search site. It contains only the search field and
				 * binds it to the test-editor web fixture field element.
				 */
				component Searchsite is Page {
					element Searchfield is field {
						label = "Search field"
						locator = "q"
						locatorStrategy = NAME
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

	private def IFile write(CharSequence contents, IProject project, String folder, String fileName, IProgressMonitor monitor) {
		createOrGetDeepFolder(project, folder)
		val file = project.getFile('''«folder»/«fileName»''')
		file.create(new StringInputStream(contents.toString), false, monitor)
		return file
	}

	private def String getGoogleTestCase() '''
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
			- Browse "http://www.google.de"
		
		* Search ^for "testeditor"
		
			Component: Searchsite
			- Enter "testeditor" into <Searchfield>
			- Press enter on <Searchfield>
		
		* Close browser
		
			Component: WebBrowser
			- Wait "3" seconds
			- Close browser
	'''

	@VisibleForTesting
	protected def String getInitialFileContents(String packageName, String... fixturesToImport) '''
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

	private def String getBuildGradleContent(String[] fixtureNames) '''
		plugins {
		    id 'org.testeditor.gradle-plugin' version '«TEST_EDITOR_GRADLE_PLUGIN_VERSION»'
		    id 'maven'
		    id 'eclipse'
		}

		group = 'org.testeditor.demo'
		version = '1.0.0-SNAPSHOT'

		// In this section you declare where to find the dependencies of your project
		repositories {
		    jcenter()
		    maven { url "http://dl.bintray.com/test-editor/Fixtures" }
		    maven { url "http://dl.bintray.com/test-editor/maven/" }
		}

		// Configure the testeditor plugin
		testeditor {
			version '«testEditorVersion»'
		}
		
		xtext {
			version '«xtextVersion»'
		}
		
		// configure logging within tests (see https://docs.gradle.org/current/dsl/org.gradle.api.tasks.testing.logging.TestLogging.html)
		// show standard out during test to see logging output
		test.testLogging.showStandardStreams = true
		// make sure that assertion failures are reported more verbose!
		test.testLogging.exceptionFormat = 'full'

		// In this section you declare the dependencies for your production and test code
		dependencies {
			compile 'org.testeditor.fixture:core-fixture:«TEST_EDITOR_CORE_FIXTURE»'
			«FOR s : fixtureNames»
				«getGradleDependency(s)»
			«ENDFOR»
			testCompile 'junit:junit:4.12'
		}
		
		// add environmental variables needed by the test (for any 'require <var-name>' in tcl)
		// test.doFirst {
		//	environment 'requiredVariable', 'value'
		// }
		
		// add proxy settings to the environment, if present (e.g. in gradle.properties)
		// keep in mind that gradle.properties w/i user ~/.gradle/gradle.properties might override project settings
		if (System.properties.containsKey('http.proxyHost')) { // set proxy properties only if present
		    test.doFirst {
		        println 'Configuring System Properties for Proxy'
		        systemProperty 'http.nonProxyHosts', System.properties['http.nonProxyHosts']
		        systemProperty 'http.proxyHost', System.properties['http.proxyHost']
		        systemProperty 'http.proxyPort', System.properties['http.proxyPort']
		        systemProperty 'http.proxyUser', System.properties['http.proxyUser']
		        systemProperty 'http.proxyPassword', System.properties['http.proxyPassword']
		        systemProperty 'https.proxyHost', System.properties['https.proxyHost']
		        systemProperty 'https.proxyPort', System.properties['https.proxyPort']
		    }
		}
		
		configurations.all {
		  resolutionStrategy {
		    forcedModules = ['org.seleniumhq.selenium:selenium-java:2.53.0'] // currently a must because of incompatibilities with more recent version
		  }
		}
	'''

	private def String getGradleDependency(String fixtureName) {
		if (fixtureName == WEBFIXTURE) {
			return '''
				compile 'org.testeditor.fixture:web-fixture:«TEST_EDITOR_WEB_FIXTURE»'
			'''
		}
		if (fixtureName == SWINGFIXTURE) {
			return '''
				compile 'org.testeditor.fixture:swing-fixture:«TEST_EDITOR_SWING_FIXTURE»'
			'''
		}
	}

	@VisibleForTesting
	protected def String getPomContent(String[] fixtureNames, String projectName) '''
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

				<testeditor.version>«testEditorVersion»</testeditor.version>
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
			</repositories>
			<pluginRepositories>
				<pluginRepository>
					<snapshots>
						<enabled>false</enabled>
					</snapshots>
					<id>test-editor-maven</id>
					<name>bintray-plugins</name>
					<url>http://dl.bintray.com/test-editor/maven</url>
				</pluginRepository>
			</pluginRepositories>

			<dependencies>
				<dependency>
					<groupId>junit</groupId>
					<artifactId>junit</artifactId>
					<version>4.12</version>
				</dependency>
				<dependency>
					<groupId>org.testeditor.fixture</groupId>
					<artifactId>core-fixture</artifactId>
					<version>«TEST_EDITOR_CORE_FIXTURE»</version>
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
										<pluginExecution>
											<pluginExecutionFilter>
												<groupId>org.testeditor</groupId>
												<artifactId>testeditor-maven-plugin</artifactId>
												<versionRange>[1.0,)</versionRange>
												<goals>
													<goal>generate</goal>
												</goals>
											</pluginExecutionFilter>
											<action>
												<ignore></ignore>
											</action>
										</pluginExecution>
									</pluginExecutions>
						          </lifecycleMappingMetadata>
							</configuration>
						</plugin>
						<plugin>
							<groupId>org.testeditor</groupId>
							<artifactId>testeditor-maven-plugin</artifactId>
							<version>«TEST_EDITOR_MAVEN_PLUGIN_VERSION»</version>
							<configuration>
								<testEditorVersion>${testeditor.version}</testEditorVersion>
								<testEditorOutput>${testeditor.output}</testEditorOutput>
								<xtextVersion>«xtextVersion»</xtextVersion>
							</configuration>
							<executions>
								<execution>
									<goals>
										<goal>generate</goal>
									</goals>
								</execution>
							</executions>
						</plugin>
					</plugins>
				</pluginManagement>
				<plugins>
					<plugin>
						<groupId>org.testeditor</groupId>
						<artifactId>testeditor-maven-plugin</artifactId>
					</plugin>
					<plugin>
						<!--
						     This is required until we have a separate task for this in the test-editor-maven-plugin.
						     We need it for the m2e lifecycle mapping (discovery of testeditor.output as a source folder).
						  -->
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

	@VisibleForTesting
	protected def String getMavenDependency(String fixtureName) {
		if (fixtureName == WEBFIXTURE) {
			return '''
				<dependency>
					<groupId>org.testeditor.fixture</groupId>
					<artifactId>web-fixture</artifactId>
					<version>«TEST_EDITOR_WEB_FIXTURE»</version>
				</dependency>
			'''
		}
		if (fixtureName == SWINGFIXTURE) {
			return '''
				<dependency>
					<groupId>org.testeditor.fixture</groupId>
					<artifactId>swing-fixture</artifactId>
					<version>«TEST_EDITOR_SWING_FIXTURE»</version>
				</dependency>
			'''
		}
	}
	
	private def void addNature(IProject newProject, String nature) {
		if (!newProject.hasNature(nature)) {
			val description = newProject.getDescription
			description.setNatureIds(description.getNatureIds + #[nature])
			newProject.setDescription(description, null)
		}
	}

	private def void createLoggingConfig(IProject project, IProgressMonitor monitor) {
		val contents = getLoggingConfigContents
		contents.write(project, RESOURCES_TEST_FOLDER, "log4j2.xml", monitor)
	}

	private def String getLoggingConfigContents() '''
		<?xml version="1.0" encoding="UTF-8"?>
		<Configuration status="INFO">
			<Appenders>
				<Console name="Console" target="SYSTEM_OUT">
					<PatternLayout pattern="%d{HH:mm:ss} %-5level [%t] %X{context} [%X{TestName}] %c{1} %msg%n" />
				</Console>
			</Appenders>
			<Loggers>
				<Logger name="org.testeditor" level="debug" additivity="false">
					<AppenderRef ref="Console" />
				</Logger>
				<Root level="info">
					<AppenderRef ref="Console" />
				</Root>
			</Loggers>
		</Configuration>
	'''

}
