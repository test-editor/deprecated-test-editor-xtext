package org.testeditor.xmllibrary.dsl.importer.xml

import javax.inject.Singleton
import org.testeditor.xmllibrary.model.ActionPart
import org.testeditor.xmllibrary.model.ModelFactory
import org.testeditor.xmllibrary.model.TechnicalBindingType
import org.testeditor.xmllibrary.model.TechnicalBindingTypes
import org.testeditor.xmllibrary.model.ActionType

/**
 * Converts {@link org.testeditor.xmllibrary.domain.binding.TechnicalBindingTypes}
 * to {@link org.testeditor.xmllibrary.model.TechnicalBindingTypes}.
 */
@Singleton
class TechnicalBindingConverter {
	
	val factory = ModelFactory.eINSTANCE
	
	def TechnicalBindingTypes convert(org.testeditor.xmllibrary.domain.binding.TechnicalBindingTypes bindings) {
		return factory.createTechnicalBindingTypes => [
			types += bindings.technicalBindingType.map[convert]
		]
	}
	
	def TechnicalBindingType convert(org.testeditor.xmllibrary.domain.binding.TechnicalBindingType binding) {
		return factory.createTechnicalBindingType => [
			id = binding.id
			name = binding.name
			if (binding.sort !== null) {
				sort = binding.sort
			}
			actionParts += binding.actionPart.map[convert]
		]
	}
	
	def ActionPart convert(org.testeditor.xmllibrary.domain.binding.ActionPart actionPart) {
		return factory.createActionPart => [
			id = actionPart.id
			value = actionPart.value
			position = actionPart.position
			type = ActionType.get(actionPart.type.value)
		]		
	}
	
}