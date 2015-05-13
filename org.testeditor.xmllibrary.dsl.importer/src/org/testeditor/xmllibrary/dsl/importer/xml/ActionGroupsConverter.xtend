package org.testeditor.xmllibrary.dsl.importer.xml

import java.util.List
import javax.inject.Singleton
import org.testeditor.xmllibrary.model.Action
import org.testeditor.xmllibrary.model.ActionGroup
import org.testeditor.xmllibrary.model.ActionGroups
import org.testeditor.xmllibrary.model.ActionPart
import org.testeditor.xmllibrary.model.Argument
import org.testeditor.xmllibrary.model.ModelFactory
import org.testeditor.xmllibrary.model.TechnicalBindingType
import org.testeditor.xmllibrary.model.TechnicalBindingTypes

/**
 * Converts {@link org.testeditor.xmllibrary.domain.action.ActionGroups}
 * to {@link org.testeditor.xmllibrary.model.ActionGroups}.
 */
@Singleton
class ActionGroupsConverter {

	val factory = ModelFactory.eINSTANCE

	def ActionGroups convert(org.testeditor.xmllibrary.domain.action.ActionGroups input, TechnicalBindingTypes bindings) {
		return factory.createActionGroups => [
			actionGroups += input.actionGroup.map[convert(bindings)]
		]
	}

	def ActionGroup convert(org.testeditor.xmllibrary.domain.action.ActionGroup input, TechnicalBindingTypes bindings) {
		return factory.createActionGroup => [
			name = input.name
			if (input.sort !== null) {
				sort = input.sort
			}
			actions += input.action.map[convert(bindings)]
		]
	}

	def Action convert(org.testeditor.xmllibrary.domain.action.Action action, TechnicalBindingTypes bindings) {
		return factory.createAction => [
			val binding = bindings.find(action.technicalBindingType)
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
			actionPart = binding.actionParts.find(argument.id)
			values += argument.value
		]
	}
	
	def TechnicalBindingType find(TechnicalBindingTypes bindings, String referencedId) {
		val types = bindings.types.filter[id == referencedId]
		if (types.empty) {
			throw new IllegalArgumentException('''Tried to find TechnicalBindingType with id=«referencedId», but found none!''')
		} else if (types.size > 1) {
			throw new IllegalArgumentException('''Tried to find TechnicalBindingType with id=«referencedId», but found multiple («types.size»).''')
		}
		return types.head
	}
	
	def ActionPart find(List<ActionPart> actionParts, String referencedId) {
		val parts = actionParts.filter[id == referencedId]
		if (parts.empty) {
			throw new IllegalArgumentException('''Tried to find ActionPart with id=«referencedId», but found none!''')
		} else if (parts.size > 1) {
			throw new IllegalArgumentException('''Tried to find ActionPart with id=«referencedId», but found multiple («parts.size»).''')
		}
		return parts.head
	}

}