package org.testeditor.tcl.dsl.ui.tests

import org.eclipse.xtext.testing.IInjectorProvider
import org.testeditor.tcl.dsl.ui.internal.DslActivator

class TclUiInjectorProvider implements IInjectorProvider {

	override getInjector() {
		return DslActivator.instance.getInjector(DslActivator.ORG_TESTEDITOR_TCL_DSL_TCL)
	}

}
