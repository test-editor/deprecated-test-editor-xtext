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
import org.testeditor.tcl.ComparatorEquals
import org.testeditor.tcl.ComparatorGreaterThan
import org.testeditor.tcl.ComparatorLessThan
import org.testeditor.tcl.ComparatorMatches
import org.testeditor.tcl.Comparison
import org.testeditor.tcl.EnvironmentVariable
import org.testeditor.tcl.Expression
import org.testeditor.tcl.StringConstant
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.VariableReferencePathAccess
import org.testeditor.tcl.AssignmentVariable

/** build a (textual) java expression based on a parsed (tcl) expression
 *  <br/><br/>
 *  this involves (currently) the generation of string constants, comparisons, variable references and variable map access.
 *  in order for variables to be resolved correctly within these expressions, a variable resolver must be set. this variable
 *  resolver is then used to determine the variable to be actually used within the generted java expression string.
 */
class TclExpressionBuilder {
	@Inject TclExpressionTypeComputer typeComputer

	def dispatch String buildExpression(Expression expression) {
		throw new RuntimeException('''no builder found for type «expression.class»''')
	}

	def String buildComparisonExpression(Expression comparisonLeftOrRight, JvmTypeReference wantedType) {
		val leftRightType = typeComputer.determineType(comparisonLeftOrRight)
		val builtLeftRightExpression = buildExpression(comparisonLeftOrRight)
		return builtLeftRightExpression.wrapWithCoercionIfNecessary(leftRightType, wantedType)
	}
	
	def dispatch String buildExpression(Comparison comparison) {
		if (comparison.comparator === null) {
			return buildExpression(comparison.left)
		}
		// check whether coercion of left or right is necessary
		
		val wantedTypeForComparison = typeComputer.coercedTypeOfComparison(comparison)
		val validTypeBuiltRightExpression = buildComparisonExpression(comparison.right, wantedTypeForComparison)
		val validTypeBuiltLeftExpression = buildComparisonExpression(comparison.left, wantedTypeForComparison)
		
		switch (comparison.comparator) {
			ComparatorEquals: '''«validTypeBuiltLeftExpression» «if(comparison.comparator.negated){'!='}else{'=='}» «validTypeBuiltRightExpression»'''
			ComparatorGreaterThan: '''«validTypeBuiltLeftExpression» «if(comparison.comparator.negated){'<='}else{'>'}» «validTypeBuiltRightExpression»'''
			ComparatorLessThan: '''«validTypeBuiltLeftExpression» «if(comparison.comparator.negated){'>='}else{'<'}» «validTypeBuiltRightExpression»'''
			ComparatorMatches: '''«validTypeBuiltLeftExpression».toString().matches(«validTypeBuiltRightExpression».toString())'''
			default:
				throw new RuntimeException('''no builder found for comparator «comparison.comparator.class»''')
		}
	}
	
	private def String wrapWithCoercionIfNecessary(String builtExpression, JvmTypeReference own, JvmTypeReference wantedType) {
		if(String.name.equals(own.qualifiedName)){
			switch( wantedType.qualifiedName) {
				case Long.name,
				case long.name: return '''Long.parseLong(«builtExpression»)'''
				case boolean.name,
				case Boolean.name: return '''Boolean.valueOf(«builtExpression»)'''
			}
		}
		return builtExpression
	}

	def dispatch String buildExpression(VariableReferencePathAccess varRef) {
		// TODO: implement json access
		// expected types of the variable is either java.util.Map or com.google.gson.JsonObject which both have the method 'get' 
		// => generated code works for both cases
		// revision: get on JsonObject will return a JsonElement which in turn must be accessed by getObject, getArray ...
		return '''«varRef.variable.variableToVarName»«varRef.path.map['.get("'+it+'")'].join»'''
	}

	def dispatch String buildExpression(VariableReference variableReference) {
		return variableReference.variable.variableToVarName
	}

	def dispatch String buildExpression(StringConstant string) {
		return '''"«string.string»"'''
	}

	def String variableToVarName(Variable variable) {
		return switch (variable) {
			EnvironmentVariable: "env_" + variable.name
			default: variable.name
		}
	}

}
