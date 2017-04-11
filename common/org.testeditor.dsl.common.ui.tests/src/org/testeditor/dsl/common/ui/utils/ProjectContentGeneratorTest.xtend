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

import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.xpath.XPathFactory
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.xtext.util.StringInputStream
import org.junit.Test
import org.mockito.InjectMocks
import org.mockito.Mock
import org.osgi.framework.FrameworkUtil
import org.osgi.framework.Version
import org.testeditor.dsl.common.testing.AbstractTest
import org.testeditor.dsl.common.util.GradleHelper

import static extension org.junit.Assume.assumeTrue
import static extension org.mockito.Matchers.*
import static extension org.mockito.Mockito.*

class ProjectContentGeneratorTest extends AbstractTest {

	@InjectMocks ProjectContentGenerator generator
	@Mock GradleHelper gradleHelper

	@Test
	def void testGetAvailableFixtureSelections() {
		// when 
		val fixtures = generator.availableFixtureNames

		// then
		assertNotNull(fixtures)
		assertTrue(fixtures.contains(ProjectContentGenerator.WEBFIXTURE))
	}

	@Test
	def void testGetAvailableBuildSystemSelections() {
		// when 
		val buildSystems = generator.availableBuildSystems

		// then
		assertNotNull(buildSystems)
		assertTrue(buildSystems.contains(ProjectContentGenerator.GRADLE))
		assertTrue(buildSystems.contains(ProjectContentGenerator.MAVEN))
	}

	@Test
	def void testGetPackageForKnownFixture() {
		// when
		val webPackage = generator.getPackage(ProjectContentGenerator.WEBFIXTURE)
		val swingPackage = generator.getPackage(ProjectContentGenerator.SWINGFIXTURE)

		// then	
		webPackage.assertEquals("org.testeditor.fixture.web")
		swingPackage.assertEquals("org.testeditor.fixture.swing")
	}

	@Test(expected=IllegalArgumentException)
	def void testGetPackageForUnkownFixture() {
		// when
		generator.getPackage("unknown")
	}

	@Test
	def void testGetMavenDependency() {
		// when
		val mavenDep = generator.getMavenDependency(ProjectContentGenerator.WEBFIXTURE)

		// then 
		assertTrue(mavenDep.contains("<groupId>org.testeditor.fixture</groupId>"))
		assertTrue(mavenDep.contains("<artifactId>web-fixture</artifactId>"))
	}

	@Test
	def void testGetMavenDependencies() {
		// given
		val fixtures = generator.availableFixtureNames

		// when
		for (fixture : fixtures) {
			// then
			val mavenDep = generator.getMavenDependency(fixture)
			assertTrue(mavenDep.contains("<groupId>"))
			assertTrue(mavenDep.contains("<artifactId>"))
		}
	}

	@Test
	def void testGetInitialAMLContent() {
		// when
		val initAml = generator.getInitialFileContents("org.example", ProjectContentGenerator.WEBFIXTURE)

		// then
		assertTrue(initAml.startsWith("package org.example"))
		assertTrue(initAml.contains("import org.testeditor.fixture.web.*"))
	}

	@Test
	def void testGetPom() {
		// given 
		val builder = DocumentBuilderFactory.newInstance.newDocumentBuilder
		val xpath = XPathFactory.newInstance.newXPath

		// when
		val pom = generator.getPomContent(#[ProjectContentGenerator.WEBFIXTURE], "MyWebProject")
		// We need to parse with an Input Stream to work well with the XML Parser in an OSGi environment
		val doc = builder.parse(new StringInputStream(pom))

		// then
		assertNotNull(doc)
		assertEquals("MyWebProject", xpath.evaluate("/project/artifactId", doc))
		assertTrue(xpath.evaluate("/project/dependencies", doc).contains("web-fixture"))
	}

	@Test
	def void testSetupEclipseMetaData() {
		// given
		val project = IProject.mock
		val monitor = IProgressMonitor.mock

		// when
		generator.setupEclipseMetaData(project, monitor)

		// then
		gradleHelper.verify.runTasks(project, "eclipse")
		project.verify.refreshLocal(IProject.DEPTH_INFINITE, monitor)
	}

	/**
	 * If an exception is thrown during the Gradle execution, we want to continue and not
	 * propagate the exception.
	 */
	@Test
	def void testSetupEclipseMetaDataWithException() {
		// given
		val project = IProject.mock
		val monitor = IProgressMonitor.mock
		doThrow(new IllegalStateException).when(gradleHelper).runTasks(project.same, any)

		// when
		generator.setupEclipseMetaData(project, monitor)

		// then
		gradleHelper.verify.runTasks(project, "eclipse")
		project.verify.refreshLocal(IProject.DEPTH_INFINITE, monitor)
	}

	@Test
	def void testGetVersion() {
		// given (this is a plugin test)
		isWithinOSGIContext.assumeTrue
		
		// when
		val version = generator.bundleVersion
		
		// then
		version.assertNotNull
		assertTrue(version.major > 0)
	}
	
	@Test
	def void testVersionMapping() {
		// given
		val version = new Version(1,2,3)
		
		// when
		val mapped = generator.mapTesteditorVersion(version)
		
		// then
		mapped.assertEquals("1.2.3")		
	}

	@Test
	def void testGetXtextVersion() {
		// given (this is a plugin test)
		isWithinOSGIContext.assumeTrue
		
		// when
		val version = generator.xtextVersion
		
		// then
		version.assertNotNull
		assumeTrue("Xtext version can be read.", version.startsWith("2."))
	}

	@Test
	def void testVersionMappingWithSNAPSHOT() {
		// given
		val version = new Version(1,2,3,"SNAPSHOT")
		
		// when
		val mapped = generator.mapTesteditorVersion(version)
		
		// then
		mapped.assertEquals("1.2.3-SNAPSHOT")		
	}

	@Test
	def void testVersionMappingWithNumberedQualifier() {
		// given
		val version = new Version(1,2,3,"201702091425")
		
		// when
		val mapped = generator.mapTesteditorVersion(version)
		
		// then
		mapped.assertEquals("1.2.3-SNAPSHOT")		
	}

	def private boolean isWithinOSGIContext() {
		val bundle = FrameworkUtil.getBundle(ProjectContentGeneratorTest)
		return bundle !== null
	}

}
