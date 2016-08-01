package org.testeditor.tcl.dsl.validation

import org.eclipse.xtext.validation.Check
import org.testeditor.tcl.MacroCollection
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.TestConfiguration

import static org.testeditor.tcl.TclPackage.Literals.*

class FileExtensionValidator extends AbstractTclValidator {

	/**
	 * Checks that the contents of a {@link TclModel} match to the their corresponding
	 * file extension.
	 * 
	 * .tcl => only a {@link TestCase} is allowed
	 * .tml => only a {@link MacroCollection} is allowed
	 * .config => only a {@link TestConfiguration} is allowed
	 */
	@Check
	def void conentsMatchFileExtension(TclModel model) {
		val fileExtension = model.eResource.URI.fileExtension
		switch (fileExtension) {
			case "tcl": {
				model.verifyNoMacroCollection
				model.verifyNoTestConfiguration
			}
			case "tml": {
				model.verifyNoTestCase
				model.verifyNoTestConfiguration
			}
			case "config": {
				model.verifyNoTestCase
				model.verifyNoMacroCollection
			}
			default:
				throw new RuntimeException('''unknown file extension (fileExtensions='«fileExtension»')''')
		}
	}

	private def void verifyNoMacroCollection(TclModel model) {
		if (model.macroCollection != null) {
			error("this file type may only contain test cases but it contains macro definitions", model,
				TCL_MODEL__MACRO_COLLECTION)
		}
	}

	private def void verifyNoTestConfiguration(TclModel model) {
		if (model.config !== null) {
			error("this file type may only contain test cases but it contains a test configuration", model,
				TCL_MODEL__CONFIG)
		}
	}

	private def void verifyNoTestCase(TclModel model) {
		if (model.test != null) {
			error("this file type may only contain macro definitions but it contains test cases", model,
				TCL_MODEL__TEST)
		}
	}

}
