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

import javax.inject.Inject
import org.eclipse.xtext.common.types.JvmTypeReference
import org.testeditor.aml.Variable
import org.testeditor.dsl.common.util.CollectionUtils
import org.testeditor.tcl.ComparatorEquals
import org.testeditor.tcl.ComparatorGreaterThan
import org.testeditor.tcl.ComparatorLessThan
import org.testeditor.tcl.ComparatorMatches
import org.testeditor.tcl.Comparison
import org.testeditor.tcl.EnvironmentVariable
import org.testeditor.tcl.Expression
import org.testeditor.tcl.JsonString
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.VariableReferencePathAccess
import org.testeditor.tcl.JsonNumber

/** build a (textual) java expression based on a parsed (tcl) expression
 *  <br/><br/>
 *  this involves (currently) the generation of string constants, comparisons, variable references and variable map access.
 *  in order for variables to be resolved correctly within these expressions, a variable resolver must be set. this variable
 *  resolver is then used to determine the variable to be actually used within the generted java expression string.
 */
class TclExpressionBuilder {

	@Inject TclExpressionTypeComputer typeComputer
	@Inject TclCoercionComputer tclCoercionComputer
	@Inject TclJsonUtil tclJsonUtil

	@Inject extension CollectionUtils

	def dispatch String buildReadExpression(Expression expression) {
		throw new RuntimeException('''no builder found for type «expression.class»''')
	}
	
	def dispatch String buildReadExpression(JsonNumber jsonNumber) {
		return jsonNumber.value
	}

	def String buildComparisonExpression(Expression compared, JvmTypeReference wantedType) {
		tclCoercionComputer.initWith(compared?.eResource)
		val typeOfCompared = typeComputer.determineType(compared, wantedType)
		val builtReadExpression = buildReadExpression(compared)
		return tclCoercionComputer.generateCoercion(wantedType, typeOfCompared, builtReadExpression)
	}
	
	def dispatch String buildReadExpression(Comparison comparison) {
		if (comparison.comparator === null) {
			return buildReadExpression(comparison.left)
		}
		// check whether coercion of left or right is necessary		
		val wantedTypeForComparison = typeComputer.coercedTypeOfComparison(comparison, null)
		val validTypeBuiltRightExpression = buildComparisonExpression(comparison.right, wantedTypeForComparison)
		val validTypeBuiltLeftExpression = buildComparisonExpression(comparison.left, wantedTypeForComparison)
		
		switch comparison.comparator {
			ComparatorEquals: '''«validTypeBuiltLeftExpression» «if(comparison.comparator.negated){'!='}else{'=='}» «validTypeBuiltRightExpression»'''
			ComparatorGreaterThan: '''«validTypeBuiltLeftExpression» «if(comparison.comparator.negated){'<='}else{'>'}» «validTypeBuiltRightExpression»'''
			ComparatorLessThan: '''«validTypeBuiltLeftExpression» «if(comparison.comparator.negated){'>='}else{'<'}» «validTypeBuiltRightExpression»'''
			ComparatorMatches: '''«validTypeBuiltLeftExpression».toString().matches(«validTypeBuiltRightExpression».toString())'''
			default:
				throw new RuntimeException('''no builder found for comparator «comparison.comparator.class»''')
		}
	}
	
	def dispatch String buildReadExpression(VariableReferencePathAccess varRef) {
		return '''«varRef.variable.variableToVarName»«varRef.path.map[tclJsonUtil.jsonPathReadAccessToString(it)].join»'''		
	}
	
	def String buildWriteExpression(VariableReferencePathAccess varRef, String assignedExpression) {
		val result = '''«varRef.variable.variableToVarName»«varRef.path.butLast.map[tclJsonUtil.jsonPathReadAccessToString(it)].join»«tclJsonUtil.jsonPathWriteAccessToString(varRef.path.last, assignedExpression)»'''
		return result
	}
		
	def dispatch String buildReadExpression(VariableReference variableReference) {
		return variableReference.variable.variableToVarName
	}

	def dispatch String buildReadExpression(JsonString string) {
		return '''"«string.value»"'''
	}

	def String variableToVarName(Variable variable) {
		return switch (variable) {
			EnvironmentVariable: "env_" + variable.name
			default: variable.name
		}
	}

}
