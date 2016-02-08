package org.testeditor.rcp4.views

import javax.inject.Inject
import org.eclipse.xtext.resource.IResourceDescriptions
import org.testeditor.aml.AmlModel
import org.testeditor.aml.dsl.ui.internal.AmlActivator

class MaskStepSelectorInput {

	@Inject
	var IResourceDescriptions resourceDescriptions;

	new() {
		val injector = AmlActivator.getInstance().getInjector(AmlActivator.ORG_TESTEDITOR_AML_DSL_AML)
		injector.injectMembers(this)
		// System.out.println(resourceDescriptions.allResourceDescriptions.map[exportedObjects].flatten.map[it.name].join(","))
		System.out.println(packages.map[name.lastSegment].join(", "))
	}

	def getPackages() {
		resourceDescriptions.allResourceDescriptions.map[exportedObjects].flatten.filter [
			EObjectOrProxy.eClass.name == AmlModel.simpleName
		]
	}

}
