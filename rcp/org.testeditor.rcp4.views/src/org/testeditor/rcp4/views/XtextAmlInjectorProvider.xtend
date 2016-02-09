package org.testeditor.rcp4.views

import org.testeditor.aml.dsl.ui.internal.DslActivator
import org.eclipse.e4.core.di.annotations.Creatable

@Creatable
class XtextAmlInjectorProvider{
	def getInjector(){
		DslActivator.instance.getInjector(DslActivator.ORG_TESTEDITOR_AML_DSL_AML)
	}
}