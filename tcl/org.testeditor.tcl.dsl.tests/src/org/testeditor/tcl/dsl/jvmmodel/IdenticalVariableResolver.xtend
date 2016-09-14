package org.testeditor.tcl.dsl.jvmmodel

import org.testeditor.tcl.VariableReference

/** resolve every variable to itself */
class IdenticalVariableResolver implements VariableResolver {
	
	override resolveVariableReference(VariableReference referencedVariable) {
		return referencedVariable
	}
	
}