package org.testeditor.tcl.dsl.naming

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.junit.Test
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import org.testeditor.tcl.dsl.tests.TclModelGenerator

class QualifiedNameProviderTest extends AbstractTclTest {

	@Inject IQualifiedNameProvider nameProvider

	@Inject extension TclModelGenerator

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

}
