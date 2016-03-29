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

import java.util.HashSet
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.xpath.XPathFactory
import org.eclipse.xtext.util.StringInputStream
import org.junit.Test

import static org.junit.Assert.*

class ProjectContentGeneratorTest {

	@Test
	def void testGetAvailableFixtureSelections() {
		// given
		val generator = new ProjectContentGenerator()
		// when 
		val fixtures = generator.availableFixtureNames
		// then
		assertNotNull(fixtures)
		assertTrue(fixtures.contains(ProjectContentGenerator.WEBFIXTURE))
	}

	@Test
	def void testGetAvailableBuildSystemSelections() {
		// given
		val generator = new ProjectContentGenerator()
		// when 
		val buildSystems = generator.availableBuildSystems
		// then
		assertNotNull(buildSystems)
		assertTrue(buildSystems.contains(ProjectContentGenerator.GRADLE))
		assertTrue(buildSystems.contains(ProjectContentGenerator.MAVEN))
	}

	@Test
	def void testGetPackgeForFixture() {
		// given 
		val generator = new ProjectContentGenerator()

		// when
		var validPackageString = generator.getPackage(ProjectContentGenerator.WEBFIXTURE)
		var invalidPackageString = generator.getPackage("")

		// then	
		assertEquals("org.testeditor.fixture.web.*", validPackageString)
		assertNotEquals("org.testeditor.fixture.web.*", invalidPackageString)
	}

	@Test
	def void testGetPackgeForEveryAvailableFixture() {
		// given 
		val generator = new ProjectContentGenerator()
		val fixtures = generator.availableFixtureNames
		val invalidPackage = generator.getPackage("")
		val packageSet = new HashSet<String>()

		// when
		for (fixture : fixtures) {
			packageSet.add(generator.getPackage(fixture))
		}

		// then
		assertFalse("No invalid package returned for a fixture name", packageSet.contains(invalidPackage))
		assertSame("Same amount of fixtures and packages", packageSet.size, fixtures.size)
	}

	@Test
	def void testGetMavenDependency() {
		// given 
		val generator = new ProjectContentGenerator()

		// when
		val mavenDep = generator.getMavenDependency(ProjectContentGenerator.WEBFIXTURE)

		// then 
		assertTrue(mavenDep.contains("<groupId>org.testeditor.fixture</groupId>"))
		assertTrue(mavenDep.contains("<artifactId>web-fixture</artifactId>"))
	}

	@Test
	def void testGetMavenDependencies() {
		// given 
		val generator = new ProjectContentGenerator()
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
		// given 
		val generator = new ProjectContentGenerator()

		// when
		val initAml = generator.getInitialAMLContent(#[ProjectContentGenerator.WEBFIXTURE], "org.example")

		// then
		assertTrue(initAml.startsWith("package org.example"))
		assertTrue(initAml.contains("import org.testeditor.fixture.web.*"))
	}

	@Test
	def void testGetPom() {
		// given 
		val generator = new ProjectContentGenerator()
		val builder = DocumentBuilderFactory.newInstance().newDocumentBuilder
		val xpath = XPathFactory.newInstance().newXPath

		// when
		val pom = generator.getPomContent(#[ProjectContentGenerator.WEBFIXTURE], "MyWebProject")
		// We need to parse with an Input Stream to work well with the XML Parser in an OSGi environment
		val doc = builder.parse(new StringInputStream(pom))

		// then
		assertNotNull(doc)
		assertEquals("MyWebProject", xpath.evaluate("/project/artifactId", doc))
		assertTrue(xpath.evaluate("/project/dependencies", doc).contains("web-fixture"))
	}

}
