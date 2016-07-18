package org.testeditor.tcl.dsl.validation

import com.google.inject.Provider
import javax.inject.Inject
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.resource.XtextResourceSet
import org.junit.Test

import static org.testeditor.tcl.TclPackage.Literals.*
import org.junit.Before

class TclModelContentValidator extends AbstractTclValidatorTest {
	@Inject protected Provider<XtextResourceSet> resourceSetProvider
	@Inject protected XtextResourceSet resourceSet
	@Inject ValidationTestHelper validator

	@Before
	def void setup() {
		resourceSet = resourceSetProvider.get
		resourceSet.classpathURIContext = this
	}

	@Test
	def void testModelContentTml() {
		val model = tclModel("Test") => [
			macroCollection = macroCollection()
		]
		model.register(resourceSet, "Test", "tml")

		validator.assertNoErrors(model)
	}

	@Test
	def void testModelContentMacroInTcl() {
		val model = tclModel("Test") => [
			macroCollection = macroCollection()
		]
		model.register(resourceSet, "Test", "tcl")

		validator.assertError(model, MACRO_COLLECTION, null)
	}

	@Test
	def void testModelContentTestCaseInTml() {
		val model = tclModel("Test") => [
			test = testCase
		]
		model.register(resourceSet, "Test", "tml")

		validator.assertError(model, TEST_CASE, null)
	}

	@Test
	def void testModelContentTcl() {
		val model = tclModel("Test") => [
			test = testCase
		]
		model.register(resourceSet, "Test", "tcl")

		validator.assertNoErrors(model)
	}

}
