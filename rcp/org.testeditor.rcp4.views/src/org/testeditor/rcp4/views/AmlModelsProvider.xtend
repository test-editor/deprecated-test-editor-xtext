package org.testeditor.rcp4.views

import javax.inject.Inject
import org.eclipse.xtext.resource.IResourceDescriptions
import org.eclipse.emf.ecore.resource.ResourceSet
import org.testeditor.aml.AmlModel
import org.eclipse.xtext.EcoreUtil2
import java.util.HashSet

public class AmlModelsProvider {
	@Inject
	var IResourceDescriptions resourceDescriptions;

	@Inject
	ResourceSet rs;

	@Inject
	AmlModelLabelProvider amlModelLabelProvider

	def Iterable<AmlModel> getModelsForNamespace(String namespace) {
		getAmlModels.filter[amlModelLabelProvider.getText(it).equals(namespace)]
	}

	def Iterable<AmlModel> getModelsForNamespaceOf(AmlModel amlModel) {
		getModelsForNamespace(amlModelLabelProvider.getText(amlModel))
	}

	def boolean empty() {
		try {
			return resourceDescriptions.empty
		} catch (Exception e) { // resource set throws an exception if the resources were not intialized (e.g. during test)
			return true
		}
	}

	def Iterable<AmlModel> getAmlModels() {
		if (empty) {
			return new HashSet
		}

		return resourceDescriptions //
		.allResourceDescriptions //
		.map[exportedObjects] //
		.flatten //
		.filter[EObjectOrProxy.eClass.name == AmlModel.simpleName] //
		.map[EObjectOrProxy] //
		.map[EcoreUtil2.resolve(it, rs) as AmlModel]
	}

}
