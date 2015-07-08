package org.testeditor.aml.dsl.generator

import org.testeditor.aml.model.ModelElement

class AbstractGenerator {
	
	/**
	 * If the label is not set we'd like to use the name as label
	 * in the generation step.
	 */
	protected def String labelOrName(ModelElement element) {
		if (!element.label.nullOrEmpty) {
			return element.label
		} else {
			return element.name
		}
	}
	
}