package org.testeditor.xmllibrary.dsl.importer.xml

import javax.inject.Singleton
import org.testeditor.xmllibrary.model.Action
import org.testeditor.xmllibrary.model.ActionGroup
import org.testeditor.xmllibrary.model.ActionGroups
import org.testeditor.xmllibrary.model.ActionPart
import org.testeditor.xmllibrary.model.Argument
import org.testeditor.xmllibrary.model.ModelFactory
import org.testeditor.xmllibrary.model.TechnicalBindingType

/**
 * Converts {@link org.testeditor.xmllibrary.domain.action.ActionGroups}
 * to {@link ActionGroups}.
 */
@Singleton
class ActionGroupsConverter {

	val factory = ModelFactory.eINSTANCE

	def ActionGroups convert(org.testeditor.xmllibrary.domain.action.ActionGroups input) {
		return factory.createActionGroups => [
			actionGroups += input.actionGroup.map[convert]
		]
	}

	def ActionGroup convert(org.testeditor.xmllibrary.domain.action.ActionGroup input) {
		return factory.createActionGroup => [
			name = input.name
			if (input.sort !== null) {
				sort = input.sort
			}
			actions += input.action.map[convert]
		]
	}

	def Action convert(org.testeditor.xmllibrary.domain.action.Action action) {
		return factory.createAction => [
			val binding = action.technicalBindingType.createDummyBinding
			technicalBindingType = binding
			if (action.actionName !== null) {
				actionNames += factory.createActionName => [
					locator = action.actionName.locator
					name = action.actionName.value
					if (action.argument !== null) {
						argument = action.argument.convert(binding)
					}
				]
			}
			if (action.sort !== null) {
				sort = action.sort
			}
		]
	}

	def Argument convert(org.testeditor.xmllibrary.domain.action.Argument argument, TechnicalBindingType binding) {
		return factory.createArgument => [
			actionPart = argument.id.createDummyActionPart
			binding.actionParts += actionPart
			values += argument.value
		]
	}

	def TechnicalBindingType createDummyBinding(String referencedId) {
		return factory.createTechnicalBindingType => [
			id = referencedId
		]
	}

	def ActionPart createDummyActionPart(String referencedId) {
		return factory.createActionPart => [
			id = referencedId
		]
	}

}