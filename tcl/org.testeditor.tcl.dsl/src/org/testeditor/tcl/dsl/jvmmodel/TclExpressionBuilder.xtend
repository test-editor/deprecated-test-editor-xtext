package org.testeditor.tcl.dsl.jvmmodel

import org.testeditor.tcl.Comparison
import org.testeditor.tcl.ComparatorEquals
import org.testeditor.tcl.ComparatorGreaterThen
import org.testeditor.tcl.ComparatorLessThen
import org.testeditor.tcl.ComparatorMatches
import org.testeditor.tcl.Expression
import org.testeditor.tcl.VariableReferenceMapAccess
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.StringConstant
import org.testeditor.aml.Variable
import org.testeditor.tcl.AssignmentVariable
import org.testeditor.tcl.EnvironmentVariable
import org.eclipse.xtend.lib.annotations.Accessors

class TclExpressionBuilder {
	
	@Accessors(PUBLIC_SETTER)
	VariableResolver variableResolver
	
	def dispatch String buildExpression(Expression expression) {
		throw new RuntimeException('''no builder found for type «expression.class»''')
	}
	
	def dispatch String buildExpression(Comparison comparison) {
		if (comparison.comparator == null) {
			return buildExpression(comparison.left)
		}
		val builtRightExpression=buildExpression(comparison.right)
		val builtLeftExpression=buildExpression(comparison.left)
		switch (comparison.comparator) {
			ComparatorEquals: '''«builtLeftExpression» «if(comparison.comparator.negated){'!='}else{'=='}» «builtRightExpression»'''
			ComparatorGreaterThen: '''«builtLeftExpression» «if(comparison.comparator.negated){'<='}else{'>'}» «builtRightExpression»'''
			ComparatorLessThen: '''«builtLeftExpression» «if(comparison.comparator.negated){'>='}else{'<'}» «builtRightExpression»'''
			ComparatorMatches: '''«builtLeftExpression».toString().matches(«builtRightExpression».toString())'''
			default:
				throw new RuntimeException('''no builder found for comparator «comparison.comparator.class»''')
		}
	}

	def dispatch String buildExpression(VariableReferenceMapAccess varRef) {
		return '''«varRef.variable.variableToVarName».get("«varRef.key»")'''
	}
	
	def dispatch String buildExpression(VariableReference varRef) {
		val stepContent=variableResolver.resolveVariableReference(varRef)
		if( stepContent instanceof VariableReference) {
			return stepContent.variable.variableToVarName
		}
		return varRef.variable.variableToVarName
	}

	def dispatch String buildExpression(StringConstant string) {
		return '''"«string.string»"'''
	}

	def String variableToVarName(Variable variable) {
		switch (variable) {
			AssignmentVariable: return variable.name
			EnvironmentVariable: return "env_" + variable.name
			default: throw new RuntimeException('''unknown variable type='«variable.class.canonicalName»'.''')
		}
	}

	
}