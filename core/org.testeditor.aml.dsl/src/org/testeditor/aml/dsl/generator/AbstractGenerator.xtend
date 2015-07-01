package org.testeditor.aml.dsl.generator

import org.testeditor.aml.model.ModelElement

class AbstractGenerator {
	
	// TODO should we do this in the generator or should the application be responsible for that?
	protected def String labelOrName(ModelElement element) {
		if (!element.label.nullOrEmpty) {
			return element.label
		} else {
			return element.name
		}
	}
	
}