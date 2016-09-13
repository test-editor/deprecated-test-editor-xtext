package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.eclipse.xtend.lib.annotations.Accessors
import org.testeditor.tcl.AssignmentVariable
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tsl.StepContent

/** resolve variables through the macro call stack to the variable originally used for the (first) call.
 * 
 *  e.g. a test step calls a macro with a variable 'x' as parameter. this parameter is used under the name
 *  'y' within the macro. calling the variable resolver setting the macroUseStack will resolve 'y' to its 
 *  original calling variable 'x'. 
 * 
 *  this functionality is used within the generated to inline macros with the right variable in the generated
 *  code. 
 */
class MacroCallVariableResolver implements VariableResolver {
	@Inject extension TclModelUtil

	@Accessors(PUBLIC_SETTER)
	Iterable<MacroTestStepContext> macroUseStack

	/**
	 * resolve dereferenced variable (in macro) with call site value (recursively if necessary).
	 * 
	 * <pre>
	 * given the following scenario (this is just one example):
	 *   Tcl uses Macro A -> which again uses a Macro B -> which uses a component interaction
	 *   => referencedVariable is the variable name in the context of B
	 *    macroUseStack = #[ B, A ]   (call usage in reverse order)
	 *    environmentVariableReferences = required environment vars of tcl (if present)
	 * 
	 * wanted:
	 *   in order to get the parameter/value that should actually be passed to the
	 *   transitively called fixture method, the value/environment variable of the
	 *   original call site within the tcl must be found.
	 * 
	 *   as long as the the macroUseStack is not empty and the parameter used for the call
	 *   is again a variable reference, this method recursively calls itself:
	 *     the referencedVariable is decoded to the parameter name as it is used in the
	 *     enclosing macro call context and the top is poped off the stack
	 *  </pre>
	 * 
	 * @see org.testeditor.tcl.dsl.validation.TclParameterUsageValidatorTest
	 * 
	 */
	// TODO: There should be a sub class of StepContent, which functions as superclass to VariableReference, StepContentVariable   
	override StepContent resolveVariableReference(VariableReference variableReference) {
		resolveVariableReference(variableReference, macroUseStack)
	}

	private def StepContent resolveVariableReference(VariableReference variableReference,
		Iterable<MacroTestStepContext> macroUseStack) {

		if (macroUseStack.empty || variableReference.variable instanceof AssignmentVariable) {
			// if the macroCallStack is empty, no further resolving is necessary
			// in case of an assignment variable, no resolving is necessary 
			return variableReference
		}

		val callSiteMacroContext = macroUseStack.head
		val macroCalled = callSiteMacroContext.findMacroDefinition
		val callSiteMacroTestStep = callSiteMacroContext.step

		if (callSiteMacroTestStep instanceof TestStep) {
			val varValMap = getVariableToValueMapping(callSiteMacroTestStep, macroCalled.template)
			val varKey = varValMap.keySet.findFirst [
				name.equals(variableReference.variable.name)
			]

			if (!varValMap.containsKey(varKey)) {
				throw new RuntimeException('''The referenced variable='«variableReference.variable.name»' cannot be resolved via macro parameters (macro call stack='«macroUseStack.map[findMacroDefinition.name].join('->')»').''')
			} else {
				val callSiteParameter = varValMap.get(varKey)

				if (callSiteParameter instanceof VariableReference) { // needs further variable resolving
					return callSiteParameter.resolveVariableReference(macroUseStack.tail)
				} else {
					return callSiteParameter // could be a StepContentVariable
				}
			}
		} else {
			throw new RuntimeException('''Call site is of type='«callSiteMacroTestStep.class.canonicalName»' but should be of type='«TestStep.canonicalName»'.''')
		}
	}
}
		