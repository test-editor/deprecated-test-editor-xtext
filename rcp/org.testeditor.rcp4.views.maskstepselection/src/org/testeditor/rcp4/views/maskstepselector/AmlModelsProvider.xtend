package org.testeditor.rcp4.views.maskstepselector

import javax.inject.Inject
import org.eclipse.xtext.resource.IResourceDescriptions
import org.eclipse.emf.ecore.resource.ResourceSet
import org.testeditor.aml.AmlModel
import org.eclipse.xtext.EcoreUtil2
import java.util.HashSet
import static org.testeditor.aml.AmlPackage.Literals.AML_MODEL

public class AmlModelsProvider {

	@Inject
	IResourceDescriptions resourceDescriptions

	@Inject
	ResourceSet rs

	def Iterable<AmlModel> getModelsForNamespace(String namespace) {
		return amlModels.filter[package == namespace]
	}

	def Iterable<AmlModel> getModelsForNamespaceOf(AmlModel amlModel) {
		return amlModel.package.modelsForNamespace
	}

	def boolean empty() {
		return resourceDescriptions.empty
	}

	def Iterable<AmlModel> getAmlModels() {
		if (empty) {
			return new HashSet
		}
		val amlDescriptions = resourceDescriptions.getExportedObjectsByType(AML_MODEL)
		amlDescriptions.forEach[]

		return resourceDescriptions //
		.allResourceDescriptions //
		.map[exportedObjects] //
		.flatten //
		.filter[EObjectOrProxy.eClass.name == AmlModel.simpleName] //
		.map[EObjectOrProxy] //
		.map[EcoreUtil2.resolve(it, rs) as AmlModel]
	}

}
