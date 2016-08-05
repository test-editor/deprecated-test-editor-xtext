package org.testeditor.tcl.dsl.validation

import javax.inject.Inject
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.junit.Before
import org.junit.Test
import org.testeditor.tcl.TclModel

import static org.testeditor.tcl.TclPackage.Literals.*
import org.testeditor.dsl.common.testing.DslParseHelper

class FileExtensionValidatorTest extends AbstractMockedTclValidatorTest {

	TclModel modelWithTestCase
	TclModel modelWithMacroCollection
	TclModel modelWithTestConfiguration

	@Before
	def void setup() {
		modelWithTestCase = tclModel => [
			test = testCase("Test")
		]
		modelWithMacroCollection = tclModel => [
			macroCollection = macroCollection("Macro")
		]
		modelWithTestConfiguration = tclModel => [
			config = testConfig("Config")
		]
	}

	@Test
	def void checkModelForTclExtension() {
		// given
		modelWithTestCase.addToResourceSet("Test.tcl")
		modelWithMacroCollection.addToResourceSet("Macro.tcl")
		modelWithTestConfiguration.addToResourceSet("Config.tcl")

		// when + then
		assertNoErrors(modelWithTestCase)
		assertError(modelWithMacroCollection, TCL_MODEL, null)
		assertError(modelWithTestConfiguration, TCL_MODEL, null)
	}

	@Test
	def void checkModelForConfigExtension() {
		// given
		modelWithTestCase.addToResourceSet("Test.config")
		modelWithMacroCollection.addToResourceSet( "Macro.config")
		modelWithTestConfiguration.addToResourceSet("Config.config")

		// when + then
		assertNoErrors(modelWithTestConfiguration)
		assertError(modelWithTestCase, TCL_MODEL, null)
		assertError(modelWithMacroCollection, TCL_MODEL, null)
	}

	@Test
	def void checkModelForTmlExtension() {
		// given
		modelWithTestCase.addToResourceSet("Test.tml")
		modelWithMacroCollection.addToResourceSet("Macro.tml")
		modelWithTestConfiguration.addToResourceSet("Config.tml")

		// when + then
		assertNoErrors(modelWithMacroCollection)
		assertError(modelWithTestConfiguration, TCL_MODEL, null)
		assertError(modelWithTestConfiguration, TCL_MODEL, null)
	}

}
