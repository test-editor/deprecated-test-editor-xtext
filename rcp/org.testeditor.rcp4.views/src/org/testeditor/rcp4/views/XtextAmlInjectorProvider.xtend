package org.testeditor.rcp4.views

import com.google.inject.Injector
import org.eclipse.e4.core.di.annotations.Creatable
import org.testeditor.aml.dsl.ui.internal.DslActivator

@Creatable
class XtextAmlInjectorProvider {
	def Injector getInjector() {
		DslActivator.instance.getInjector(DslActivator.ORG_TESTEDITOR_AML_DSL_AML)
	}
}
