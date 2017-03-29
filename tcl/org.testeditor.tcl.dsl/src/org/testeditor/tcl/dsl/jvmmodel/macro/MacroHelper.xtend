package org.testeditor.tcl.dsl.jvmmodel.macro

import java.util.Set
import com.google.inject.Inject
import org.testeditor.tcl.Macro
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.SetupAndCleanupProvider
import org.testeditor.tcl.StepContainer
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.util.TclModelUtil

class MacroHelper {

	@Inject MacroMethodNameStore nameStore
	@Inject extension TclModelUtil

	/**
	 * @return the method name that shall be used for the given macro
	 */
	def String getMethodName(Macro macro) {
		return nameStore.getMethodName(macro)
	}

	/**
	 * Collects all macros used by a {@link SetupAndCleanupProvider}.
	 */
	def Set<Macro> getAllTransitiveMacros(SetupAndCleanupProvider element) {
		val result = newHashSet
		element.setup?.collectUsedMacros(result)
		element.cleanup?.collectUsedMacros(result)
		if (element instanceof TestCase) {
			element.steps.forEach[collectUsedMacros(result)]
		}
		return result
	}

	private def void collectUsedMacros(StepContainer stepContainer, Set<Macro> result) {
		val macroContexts = stepContainer.contexts.filter(MacroTestStepContext)
		for (context : macroContexts) {
			val macros = context.steps.filter(TestStep).map[findMacroDefinition(context)].filterNull
			for (macro : macros) {
				result += macro
				collectUsedMacros(macro, result)
			}
		}
	}

}
