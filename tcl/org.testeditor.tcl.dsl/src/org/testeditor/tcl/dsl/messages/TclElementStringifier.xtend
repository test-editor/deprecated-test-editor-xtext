package org.testeditor.tcl.dsl.messages

import javax.inject.Inject
import javax.inject.Singleton
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.slf4j.LoggerFactory
import org.testeditor.tcl.AssertionTestStep
import org.testeditor.tcl.AssignmentThroughPath
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.Expression
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.TestStepWithAssignment
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tsl.SpecificationStep

import static extension org.eclipse.xtext.nodemodel.util.NodeModelUtils.getNode
import static extension org.eclipse.xtext.nodemodel.util.NodeModelUtils.getTokenText

@Singleton
class TclElementStringifier {

	static val logger = LoggerFactory.getLogger(TclElementStringifier)

	@Inject extension TclModelUtil

	def dispatch String stringify(TestCase it) {
		return name
	}

	/**
	 * Get the textual representation of the expression as is
	 */
	def String assertionText(Expression expression) {
		return NodeModelUtils.getNode(expression)?.text?.trim ?: ""
	}

	// SpecificationStep and TestStep each define 'contents' as a field of type EList<StepContent>
	// but its  not in a common super-type, because of different semantics
	def dispatch String stringify(SpecificationStep it) {
		val node = getNode
		return if (node !== null) {
			val text = node.tokenText
			val relevantText = text.split('\n').map[trim].takeWhile[!startsWith("Component") && !startsWith("Mask") && !startsWith("Macro")].join(' ')
			relevantText.substring(text.indexOf('*') + 1)
		} else
			{
				contents.restoreString
			}.trim
	}

	def dispatch String stringify(TestStep it) {
		return stringifyTestStep
	}

	def dispatch String stringify(TestStepWithAssignment it) {
		return '''«variable.name» = «stringifyTestStep» «typeInfoSuffix»'''
	}

	def dispatch String stringify(AssertionTestStep it) {
		return '''assert «assertionText(assertExpression)»'''
	}

	def dispatch String stringify(AssignmentThroughPath it) {
		return '''«variableReference?.restoreString» = «assertionText(expression)»'''
	}

	def dispatch String stringify(ComponentTestStepContext it) {
		return component.name
	}

	def dispatch String stringify(MacroTestStepContext it) {
		return macroCollection.name
	}

	def dispatch String stringify(EObject it) {
		logger.warn('''Don't know how to stringify elements of type «it.class.name»; using simple class name.''')
		return class.simpleName
	}
	
	def String typeInfoSuffix(TestStepWithAssignment it) {
		var typeInfoSuffix = ''
		if (hasComponentContext) {
			val operation = interaction.defaultMethod?.operation
			if (operation !== null) {
				typeInfoSuffix = '''[«operation.returnType.identifier»]'''
			} else {
				logger.warn('''Interaction type '«interaction.name»' does not have a proper method reference.''')
			}
		}
		return typeInfoSuffix
	}

	def stringifyTestStep(TestStep it) {
		return contents.restoreString.trim
	}

}
