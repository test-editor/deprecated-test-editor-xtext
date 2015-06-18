package org.testeditor.aml.dsl.validation

import javax.inject.Inject
import org.eclipse.xtext.validation.Check
import org.testeditor.aml.model.Component
import org.testeditor.aml.model.ModelUtil

import static org.testeditor.aml.model.ModelPackage.Literals.*

class AmlValidator extends AbstractAmlValidator {

	public static val COMPONENT__PARENTS__CYCLE = "component.parents.cycle"
	public static val COMPONENT__TYPE__MISSING = 'component.type.missing'

	@Inject
	private extension ModelUtil

	/**
	 * Checks that a {@link Component} does not have a cycle in its parents hierarchy.
	 */
	@Check
	def void checkComponentHasNoCycle(Component component) {
		if (component.hasParentsCycle) {
			error(
				"Component has a cycle in its includes hierarchy.",
				COMPONENT__PARENTS,
				COMPONENT__PARENTS__CYCLE
			)
		}
	}

	/**
	 * Checks that a {@link Component} has a type (declared or inherited).
	 */
	@Check
	def void checkComponentHasType(Component component) {
		if (!component.hasParentsCycle && component.types.empty) {
			error(
				"Component has no type.",
				COMPONENT__TYPE,
				COMPONENT__TYPE__MISSING
			)
		}
	}

}
