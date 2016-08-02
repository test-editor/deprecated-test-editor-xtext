package org.testeditor.tcl.dsl.validation

import com.google.inject.Provider
import javax.inject.Inject
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.resource.XtextResourceSet
import org.junit.Before
import org.junit.Test
import org.testeditor.tcl.TclModel

import static org.testeditor.tcl.TclPackage.Literals.*

class FileExtensionValidatorTest extends AbstractTclValidatorTest {

	@Inject protected Provider<XtextResourceSet> resourceSetProvider
	@Inject protected XtextResourceSet resourceSet
	@Inject ValidationTestHelper validator

	TclModel modelWithTestCase
	TclModel modelWithMacroCollection
	TclModel modelWithTestConfiguration

	@Before
	def void setup() {
		resourceSet = resourceSetProvider.get
		resourceSet.classpathURIContext = this
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
		modelWithTestCase.register(resourceSet, "Test.tcl")
		modelWithMacroCollection.register(resourceSet, "Macro.tcl")
		modelWithTestConfiguration.register(resourceSet, "Config.tcl")

		// when + then
		validator.assertNoErrors(modelWithTestCase)
		validator.assertError(modelWithMacroCollection, TCL_MODEL, null)
		validator.assertError(modelWithTestConfiguration, TCL_MODEL, null)
	}

	@Test
	def void checkModelForConfigExtension() {
		// given
		modelWithTestCase.register(resourceSet, "Test.config")
		modelWithMacroCollection.register(resourceSet, "Macro.config")
		modelWithTestConfiguration.register(resourceSet, "Config.config")

		// when + then
		validator.assertNoErrors(modelWithTestConfiguration)
		validator.assertError(modelWithTestCase, TCL_MODEL, null)
		validator.assertError(modelWithMacroCollection, TCL_MODEL, null)
	}

	@Test
	def void checkModelForTmlExtension() {
		// given
		modelWithTestCase.register(resourceSet, "Test.tml")
		modelWithMacroCollection.register(resourceSet, "Macro.tml")
		modelWithTestConfiguration.register(resourceSet, "Config.tml")

		// when + then
		validator.assertNoErrors(modelWithMacroCollection)
		validator.assertError(modelWithTestConfiguration, TCL_MODEL, null)
		validator.assertError(modelWithTestConfiguration, TCL_MODEL, null)
	}

}
