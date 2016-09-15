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
package org.testeditor.tcl.dsl.validation

import java.util.List
import org.eclipse.emf.ecore.EPackage
import org.eclipse.xtext.validation.AbstractDeclarativeValidator
import org.eclipse.xtext.validation.Check
import org.testeditor.tcl.MacroCollection
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TclPackage
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.TestConfiguration

import static org.testeditor.tcl.TclPackage.Literals.*

class FileExtensionValidator extends AbstractDeclarativeValidator {

	override protected List<EPackage> getEPackages() {
		return #[TclPackage.eINSTANCE]
	}

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
			error("This file type may not contain macro collections.", model, TCL_MODEL__MACRO_COLLECTION)
		}
	}

	private def void verifyNoTestConfiguration(TclModel model) {
		if (model.config !== null) {
			error("This file type may not contain test configurations.", model, TCL_MODEL__CONFIG)
		}
	}

	private def void verifyNoTestCase(TclModel model) {
		if (model.test != null) {
			error("This file type may not contain test cases.", model, TCL_MODEL__TEST)
		}
	}

}
