package org.testeditor.aml.model

import java.util.HashSet
import java.util.Set

class ModelUtil {

	// TODO implement hasParentsCycle and test!
	public static final String TEMPLATE_VARIABLE_ELEMENT = "element"

	/**
	 * Checks whether the {@link Component}'s parent hierarchy
	 * contains a cycle or not.
	 */
	def boolean hasParentsCycle(Component component) {
		return false
	}

	def Set<ComponentType> getTypes(Component component) {
		val result = new HashSet
		component.collectTypes(result)
		return result
	}

	private def void collectTypes(Component component, Set<ComponentType> result) {
		if (component.type !== null) {
			result += component.type
		}
		component.parents.forEach[collectTypes(result)]
	}

	/**
	 * @return all {@link TemplateVariable variables} that can be referenced
	 * 	from the outside, i.e. have a name that is not "element"
	 */
	def Set<TemplateVariable> getReferenceableVariables(Template template) {
		return template.contents.filter(TemplateVariable).filter[
			!name.nullOrEmpty && name != TEMPLATE_VARIABLE_ELEMENT
		].toSet
	}

}