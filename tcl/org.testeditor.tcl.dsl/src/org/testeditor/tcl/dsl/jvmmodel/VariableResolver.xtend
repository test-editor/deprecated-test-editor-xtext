package org.testeditor.tcl.dsl.jvmmodel

import org.testeditor.tsl.StepContent
import org.testeditor.tcl.VariableReference

/** provide an interface for resolving variables 
 * (e.g. in calls to macros the variable of the call location must be used, not the parameter (name) of the macro definition itself)
 */
interface VariableResolver {
	
	def StepContent resolveVariableReference(VariableReference referencedVariable)	
	
}