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
package org.testeditor.tcl.dsl.naming

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.junit.Test
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import org.testeditor.tcl.dsl.tests.TclModelGenerator
import org.mockito.Mock

import static org.mockito.Mockito.*
import org.mockito.InjectMocks
import org.testeditor.dsl.common.util.classpath.ClasspathUtil

class QualifiedNameProviderTest extends AbstractTclTest {

	@Inject IQualifiedNameProvider nameProvider

	@Inject extension TclModelGenerator

	@Mock ClasspathUtil classpathUtil
	
	@InjectMocks TclQualifiedNameProvider tclNameProvider

	private def void assertFQN(EObject element, String expectedName) {
		// when
		val actualName = nameProvider.getFullyQualifiedName(element)

		// then
		actualName.toString.assertEquals(expectedName)
	}

	@Test
	def void nameForTclModel() {
		// given
		val model = tclModel => [
			package = "com.example"
		]

		// when + then
		model.assertFQN("com.example")
	}

	@Test
	def void nameForTestCase() {
		// given
		val model = tclModel => [
			package = "com.example"
			test = testCase("MyTest")
		]

		// when + then
		model.test.assertFQN("com.example.MyTest")
	}

	@Test
	def void nameForTestConfiguration() {
		// given
		val model = tclModel => [
			package = "com.example"
			config = testConfig("MyConfig")
		]

		// when + then
		model.config.assertFQN("com.example.MyConfig")
	}

	@Test
	def void nameForMacroCollection() {
		// given
		val model = tclModel => [
			package = "com.example"
			macroCollection = macroCollection("MyMacros")
		]

		// when + then
		model.macroCollection.assertFQN("com.example.MyMacros")
	}

	@Test
	def void testNameForTestCaseWithoutPackageKeyword() {
		// given
		when(classpathUtil.inferPackage(any(EObject))).thenReturn("my.package")

		val model = tclModel => [
			package = null
			test = testCase("MyTest")
		]

		// when + then
		assertEquals("my.package", tclNameProvider.qualifiedName(model).toString)
	}

}
