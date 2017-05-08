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
import org.testeditor.tcl.JsonString
import org.testeditor.tcl.KeyPathElement
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.VariableReferencePathAccess
import org.testeditor.tcl.AccessPathElement
import org.testeditor.tcl.ArrayPathElement
import org.testeditor.dsl.common.util.CollectionUtils

/** build a (textual) java expression based on a parsed (tcl) expression
 *  <br/><br/>
 *  this involves (currently) the generation of string constants, comparisons, variable references and variable map access.
 *  in order for variables to be resolved correctly within these expressions, a variable resolver must be set. this variable
 *  resolver is then used to determine the variable to be actually used within the generted java expression string.
 */
class TclExpressionBuilder {

	@Inject TclExpressionTypeComputer typeComputer

	@Inject extension CollectionUtils

	def dispatch String buildReadExpression(Expression expression) {
		throw new RuntimeException('''no builder found for type «expression.class»''')
	}

	def String buildComparisonExpression(Expression compared, JvmTypeReference wantedType) {
		val typeOfCompared = typeComputer.determineType(compared)
		val builtReadExpression = buildReadExpression(compared)
		return builtReadExpression.wrapWithCoercionIfNecessary(typeOfCompared, wantedType)
	}
	
	def dispatch String buildReadExpression(Comparison comparison) {
		if (comparison.comparator === null) {
			return buildReadExpression(comparison.left)
		}
		// check whether coercion of left or right is necessary
		
		val wantedTypeForComparison = typeComputer.coercedTypeOfComparison(comparison)
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
	
	private def String wrapWithCoercionIfNecessary(String builtExpression, JvmTypeReference own, JvmTypeReference wantedType) {
		if(String.name.equals(own.qualifiedName)){
			switch (wantedType.qualifiedName) {
				case Long.name,
				case long.name: return '''Long.parseLong(«builtExpression»)'''
				case boolean.name,
				case Boolean.name: return '''Boolean.valueOf(«builtExpression»)'''
				case String.name: return builtExpression					
			}
		} else if (typeComputer.isJsonType(own)) {
			switch (wantedType.qualifiedName) {
				case Long.name,
				case long.name: return '''«builtExpression».getAsJsonPrimitive().getAsLong()'''
				case boolean.name,
				case Boolean.name: return '''«builtExpression».getAsJsonPrimitive().getAsBoolean()'''
				case String.name: return '''«builtExpression».getAsJsonPrimitive().getAsString()'''
			}
		} 
		return builtExpression
	}

	def dispatch String buildReadExpression(VariableReferencePathAccess varRef) {
		return '''«varRef.variable.variableToVarName»«varRef.path.map[jsonPathReadAccessToString].join»'''		
	}
	
	def String buildWriteExpression(VariableReferencePathAccess varRef, String assignedExpression) {
		val result = '''«varRef.variable.variableToVarName»«varRef.path.butLast.map[jsonPathReadAccessToString].join»«varRef.path.last.jsonPathWriteAccessToString(assignedExpression)»'''
		return result
	}
		
	private def String jsonPathReadAccessToString(AccessPathElement pathElement) {
		switch (pathElement) {
			KeyPathElement: return '''.getAsJsonObject().get("«pathElement.key»")'''
			ArrayPathElement: return '''.getAsJsonArray().get(«pathElement.number»)'''
			default: throw new RuntimeException('''Unknown path element type = '«pathElement.class.name»'.''')
		}
	}
	
	private def String jsonPathWriteAccessToString(AccessPathElement pathElement, String value) {
		switch (pathElement) {
			KeyPathElement: return '''.getAsJsonObject().add("«pathElement.key»", «value»)'''
			ArrayPathElement: return '''.getAsJsonArray().set(«pathElement.number», «value»)'''
			default: throw new RuntimeException('''Unknown path element type = '«pathElement.class.name»'.''')
		}
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
