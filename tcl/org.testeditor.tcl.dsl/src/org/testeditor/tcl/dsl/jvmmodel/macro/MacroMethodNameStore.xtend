package org.testeditor.tcl.dsl.jvmmodel.macro

import java.util.Map
import org.testeditor.tcl.Macro

import static extension org.eclipse.xtext.EcoreUtil2.getContainerOfType
import org.testeditor.tcl.MacroCollection

/**
 * Helper class to calculate method names for a macro and store a mapping
 * of a macro to its name in case there are duplicates.
 */
class MacroMethodNameStore {

	Map<Macro, String> macroToName = newHashMap

	/**
	 * @return the method name that shall be used for the given macro
	 */
	def synchronized String getMethodName(Macro macro) {
		return macroToName.computeIfAbsent(macro, [uniqueMethodName])
	}

	private def String getUniqueMethodName(Macro macro) {
		val defaultName = macro.defaultMethodName
		var newMethodName = defaultName
		var i = 1
		while (macroToName.containsValue(newMethodName)) {
			newMethodName = '''«defaultName»_«i++»'''
		}
		return newMethodName
	}

	private def String getDefaultMethodName(Macro macro) {
		return '''macro_«macro.getContainerOfType(MacroCollection).name»_«macro.name»'''
	}

}
