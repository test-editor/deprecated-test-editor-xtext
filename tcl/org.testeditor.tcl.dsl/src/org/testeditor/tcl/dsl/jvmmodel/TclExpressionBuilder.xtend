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

import java.util.Optional
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
import org.testeditor.tcl.JsonBoolean
import org.testeditor.tcl.JsonNull
import org.testeditor.tcl.JsonNumber
import org.testeditor.tcl.JsonString
import org.testeditor.tcl.JsonValue
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.VariableReferencePathAccess

/**
 * build a (textual) java expression based on a parsed (tcl) expression
 */
class TclExpressionBuilder {

	@Inject TclExpressionTypeComputer typeComputer
	@Inject TclCoercionComputer coercionComputer
	@Inject TclJsonUtil jsonUtil

	@Inject extension CollectionUtils

	def String buildWriteExpression(VariableReferencePathAccess varRef, String assignedExpression) {
		val result = '''«varRef.variable.variableToVarName»«varRef.path.butLast.map[jsonUtil.jsonPathReadAccessToString(it)].join»«jsonUtil.jsonPathWriteAccessToString(varRef.path.last, assignedExpression)»'''
		return result
	}
		
	def dispatch String buildReadExpression(Expression expression) {
		throw new RuntimeException('''no builder found for type «expression.class»''')
	}
	
	def dispatch String buildReadExpression(Comparison comparison) {
		if (comparison.comparator === null) {
			return buildReadExpression(comparison.left)
		}
		// check whether coercion of left or right is necessary		
		val wantedTypeForComparison = typeComputer.coercedTypeOfComparison(comparison, Optional.empty)
		val validTypeBuiltRightExpression = buildReadExpression(comparison.right, wantedTypeForComparison)
		val validTypeBuiltLeftExpression = buildReadExpression(comparison.left, wantedTypeForComparison)
		
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
		return '''«varRef.variable.variableToVarName»«varRef.path.map[jsonUtil.jsonPathReadAccessToString(it)].join»'''
	}
	
	def dispatch String buildReadExpression(VariableReference variableReference) {
		return variableReference.variable.variableToVarName
	}

	def dispatch String buildReadExpression(JsonValue jsonValue) {
		switch jsonValue {
			JsonBoolean: return jsonValue.value.toString
			JsonString: return '''"«jsonValue.value»"'''
			JsonNull: return 'null'
			JsonNumber: return jsonValue.value
			default: throw new RuntimeException('''Cannot build read expression for json value of type = '«jsonValue?.class»'.''')
		}
	}	

	def String buildReadExpression(Expression compared, JvmTypeReference wantedType) {
		coercionComputer.initWith(compared?.eResource)
		val typeOfCompared = typeComputer.determineType(compared, Optional.ofNullable(wantedType))
		val builtExpression = buildReadExpression(compared)
		return coercionComputer.generateCoercion(wantedType, typeOfCompared, builtExpression)
	}
	
	def String variableToVarName(Variable variable) {
		return switch (variable) {
			EnvironmentVariable: "env_" + variable.name
			default: variable.name
		}
	}

}
