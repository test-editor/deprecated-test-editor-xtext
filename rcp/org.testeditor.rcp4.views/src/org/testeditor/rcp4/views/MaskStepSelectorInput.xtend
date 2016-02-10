package org.testeditor.rcp4.views

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.resource.IResourceDescriptions
import org.testeditor.aml.AmlModel

class MaskStepSelectorInput {

	@Inject
	var IResourceDescriptions resourceDescriptions;

	def Iterable<EObject> getPackages() {
		resourceDescriptions.allResourceDescriptions.map[exportedObjects].flatten.filter [
			EObjectOrProxy.eClass.name == AmlModel.simpleName
		].map[EObjectOrProxy]
	}

}
