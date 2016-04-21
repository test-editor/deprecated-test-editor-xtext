package org.testeditor.tcl.dsl.ui.tests

import org.eclipse.xtext.junit4.IInjectorProvider
import org.testeditor.tcl.dsl.ui.internal.DslActivator

class TclUiInjectorProvider implements IInjectorProvider {

	override getInjector() {
		return DslActivator.instance.getInjector(DslActivator.ORG_TESTEDITOR_TCL_DSL_TCL)
	}

}
