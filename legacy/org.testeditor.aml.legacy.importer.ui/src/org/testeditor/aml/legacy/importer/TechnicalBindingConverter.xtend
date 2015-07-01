package org.testeditor.aml.legacy.importer

import java.util.List
import javax.inject.Singleton
import org.testeditor.aml.model.InteractionType
import org.testeditor.aml.model.ModelFactory
import org.testeditor.aml.model.TemplateContent
import org.testeditor.xmllibrary.domain.binding.ActionPart
import org.testeditor.xmllibrary.domain.binding.TechnicalBindingType
import org.testeditor.xmllibrary.domain.binding.TechnicalBindingTypes

/**
 * Converts {@link TechnicalBindingType} into {@link InteractionType}.
 */
@Singleton
class TechnicalBindingConverter {
	
	static val factory = ModelFactory.eINSTANCE
	static val ACTION_NAME_VARIABLE_NAME = "element" // TODO inject with named annotation
	
	def List<InteractionType> convert(TechnicalBindingTypes technicalBindings) {
		return technicalBindings.technicalBindingType.map[convert].toList	
	}
	
	def InteractionType convert(TechnicalBindingType technicalBinding) {
		val result = factory.createInteractionType
		result => [
			name = technicalBinding.id
			label = technicalBinding.name
			template = factory.createTemplate
			template.contents += technicalBinding.actionPart.map[convert]
		]
		return result
	}
	
	protected def TemplateContent convert(ActionPart actionPart) {
		return switch actionPart.type {
			case TEXT: factory.createTemplateText => [
				value = actionPart.value
			]
			case ACTION_NAME: factory.createTemplateVariable => [
				name = ACTION_NAME_VARIABLE_NAME
			]
			case ARGUMENT: factory.createTemplateVariable => [
				name = actionPart.id ?: ""
			]
		}
	}
	
}