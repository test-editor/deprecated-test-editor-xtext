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

import org.junit.Test
import static org.junit.Assert.*
import java.util.HashSet
import javax.xml.parsers.DocumentBuilderFactory
import java.io.ByteArrayInputStream
import javax.xml.xpath.XPathFactory

class ProjectContentGeneratorTest {

	@Test
	def testGetAvailableSelections() {
		// given
		val generator = new ProjectContentGenerator()
		// when 
		val buildSystems = generator.availableBuildSystems
		val fixtures = generator.availableFixtureNames
		// then
		assertNotNull(buildSystems)
		assertNotNull(fixtures)
		assertTrue(buildSystems.contains(ProjectContentGenerator.GRADLE))
		assertTrue(buildSystems.contains(ProjectContentGenerator.MAVEN))
		assertTrue(fixtures.contains(ProjectContentGenerator.WEBFIXTURE))
	}

	@Test
	def testGetPackgeForFixture() {
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
	def testGetPackgeForEveryAvailableFixture() {
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
	def testGetMavenDependency() {
		// given 
		val generator = new ProjectContentGenerator()

		// when
		val mavenDep = generator.getMavenDependency(ProjectContentGenerator.WEBFIXTURE)

		// then 
		assertTrue(mavenDep.contains("<groupId>org.testeditor.fixture</groupId>"))
		assertTrue(mavenDep.contains("<artifactId>web-fixture</artifactId>"))
	}

	@Test
	def testGetMavenDependencies() {
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
	def testGetInitialAMLContent() {
		// given 
		val generator = new ProjectContentGenerator()

		// when
		val initAml = generator.getInitialAMLContent(#[ProjectContentGenerator.WEBFIXTURE], "org.example")

		// then
		assertTrue(initAml.startsWith("package org.example"))
		assertTrue(initAml.contains("import org.testeditor.fixture.web.*"))
	}

	@Test
	def testGetPom() {
		// given 
		val generator = new ProjectContentGenerator()
		val builder = DocumentBuilderFactory.newInstance().newDocumentBuilder
		val xpath = XPathFactory.newInstance().newXPath

		// when
		val pom = generator.getPomContent(#[ProjectContentGenerator.WEBFIXTURE], "MyWebProject")
		val doc = builder.parse(pom)

		// then
		assertNotNull(doc)
		assertEquals("MyWebProject", xpath.evaluate("/project/artifactId", doc))
		assertTrue(xpath.evaluate("/project/dependencies", doc).contains("web-fixture"))
	}

}
