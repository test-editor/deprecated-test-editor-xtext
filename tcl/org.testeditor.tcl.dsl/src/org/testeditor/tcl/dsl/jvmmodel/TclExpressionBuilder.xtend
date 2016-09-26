/*******************************************************************************
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 *******************************************************************************/
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
import org.slf4j.LoggerFactory

/** build a (textual) java expression based on a parsed (tcl) expression
 *  <br/><br/>
 *  this involves (currently) the generation of string constants, comparisons, variable references and variable map access.
 *  in order for variables to be resolved correctly within these expressions, a variable resolver must be set. this variable
 *  resolver is then used to determine the variable to be actually used within the generted java expression string.
 */
class TclExpressionBuilder {
	
	static val logger = LoggerFactory.getLogger(TclExpressionBuilder)

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
		val result = if (stepContent instanceof VariableReference) {
				stepContent.variable.variableToVarName
			} else {
				varRef.variable.variableToVarName
			}
		logger.trace("resolved variable reference='{}' to step content='{}'", varRef.variable.name, result)
		return result
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