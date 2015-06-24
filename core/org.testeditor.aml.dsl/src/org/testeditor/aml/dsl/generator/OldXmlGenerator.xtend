package org.testeditor.aml.dsl.generator

import javax.inject.Inject
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.generator.IFileSystemAccess
import org.eclipse.xtext.generator.IGenerator
import org.testeditor.aml.model.AmlModel

class OldXmlGenerator implements IGenerator {

	@Inject extension AllActionGroupsGenerator
	@Inject extension TechnicalBindingsGenerator

	override doGenerate(Resource input, IFileSystemAccess fsa) {
		val model = input.contents.head as AmlModel
		if (!model.fileNameAllActionGroups.nullOrEmpty) {
			fsa.generateFile(model.fileNameAllActionGroups, model.generateAllActionGroups)
		}
		if (!model.fileNameTechnicalBindings.nullOrEmpty) {
			fsa.generateFile(model.fileNameTechnicalBindings, model.generateTechnicalBindings)
		}
	}

}