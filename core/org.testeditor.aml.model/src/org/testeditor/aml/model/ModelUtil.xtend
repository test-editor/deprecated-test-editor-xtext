package org.testeditor.aml.model

import java.util.HashSet
import java.util.Set

class ModelUtil {
	
	// TODO implement hasParentsCycle and test!
	
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
		result += component.type
		component.parents.forEach[collectTypes(result)]
	}
	
}