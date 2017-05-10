/*******************************************************************************
 * Copyright (c) 2012 - 2017 Signal Iduna Corporation and others.
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

import java.util.regex.Pattern
import javax.inject.Inject
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.common.types.JvmTypeReference
import org.testeditor.aml.TemplateContainer
import org.testeditor.aml.TemplateVariable
import org.testeditor.aml.Variable
import org.testeditor.tcl.AssignmentVariable
import org.testeditor.tcl.ComparatorEquals
import org.testeditor.tcl.ComparatorGreaterThan
import org.testeditor.tcl.ComparatorLessThan
import org.testeditor.tcl.ComparatorMatches
import org.testeditor.tcl.Comparison
import org.testeditor.tcl.EnvironmentVariable
import org.testeditor.tcl.Expression
import org.testeditor.tcl.JsonArray
import org.testeditor.tcl.JsonNumber
import org.testeditor.tcl.JsonObject
import org.testeditor.tcl.JsonString
import org.testeditor.tcl.NullOrBoolCheck
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.VariableReferencePathAccess
import org.testeditor.tcl.dsl.validation.TclTypeValidationUtil
import org.testeditor.tsl.StepContent
import org.testeditor.tsl.StepContentVariable

class TclExpressionTypeComputer {
	
	@Inject SimpleTypeComputer typeComputer
	@Inject TclTypeValidationUtil typeValidationUtil		
	@Inject TclJsonUtil jsonUtil
	@Inject TclJvmTypeReferenceUtil tclJvmTypeReferenceUtil
	@Inject TclCoercionComputer tclCoercionComputer
	
	def boolean isJsonType(Expression expression) {
		switch expression {
			// com.google.gson.JsonElement: return true // supertype of all (relevant) json types (e.g. JsonObject, JsonArray, JsonString ...)
			VariableReferencePathAccess: return true // since this is only allowed for json types, the result is a json type, too
			VariableReference: return jsonUtil.isJsonType(determineType(expression, null))
			default: return false
		}
	}
	
	def dispatch JvmTypeReference determineType(StepContent stepContent, JvmTypeReference expectedType) {
		tclJvmTypeReferenceUtil.initWith(stepContent.eResource)
		val longPattern = Pattern.compile("[0-9]+")
		val booleanPattern = Pattern.compile("true|false", Pattern.CASE_INSENSITIVE)
		switch stepContent {			
			StepContentVariable: {
				if(expectedType !== null) {
					if (tclJvmTypeReferenceUtil.isString(expectedType)) {
						return expectedType
					}
					if (tclJvmTypeReferenceUtil.isBoolean(expectedType) && booleanPattern.matcher(stepContent.value).matches) {
						return expectedType
					}
					if (tclJvmTypeReferenceUtil.isNonFractionalNumber(expectedType) && longPattern.matcher(stepContent.value).matches) {
						return expectedType
					}
				} 
				// (fallback) try to find out without context information
				if (longPattern.matcher(stepContent.value).matches) {
					return tclJvmTypeReferenceUtil.longObjectJvmTypeReference
				} else if (booleanPattern.matcher(stepContent.value).matches) {
					return tclJvmTypeReferenceUtil.booleanObjectJvmTypeReference
				} else {
					return tclJvmTypeReferenceUtil.stringJvmTypeReference
				}
			}
			VariableReference: return determineType(stepContent.variable, expectedType)
			default: throw new RuntimeException('''Unknown step content type = '«stepContent.class.name»' for type determination.''')
		}
		
	}
	
	def dispatch JvmTypeReference determineType(Expression expression, JvmTypeReference expectedType) {
		tclJvmTypeReferenceUtil.initWith(expression.eResource)
		switch expression {
			VariableReferencePathAccess: return tclJvmTypeReferenceUtil.jsonElementJvmTypeReference
			JsonObject: return tclJvmTypeReferenceUtil.jsonObjectJvmTypeReference
			JsonArray: return tclJvmTypeReferenceUtil.jsonArrayJvmTypeReference
			JsonNumber: return tclJvmTypeReferenceUtil.longObjectJvmTypeReference // TODO should be big decimal
			JsonString: return tclJvmTypeReferenceUtil.stringJvmTypeReference
			VariableReference: return expression.variable.determineType(expectedType)
			Comparison: if(expression.comparator === null) {
				expression.left.determineType(expectedType)
			} else {
				return tclJvmTypeReferenceUtil.booleanPrimitiveJvmTypeReference
			}
			NullOrBoolCheck: return tclJvmTypeReferenceUtil.booleanPrimitiveJvmTypeReference
			default: { throw new RuntimeException("Expression of type '"+expression.class.canonicalName+"' is unknown")}
		}
	}
	
	def dispatch JvmTypeReference determineType(Variable variable, JvmTypeReference expectedType) {
		tclJvmTypeReferenceUtil.initWith(variable.eResource)
		switch variable {
			AssignmentVariable : return typeValidationUtil.determineType(variable)
			EnvironmentVariable : return tclJvmTypeReferenceUtil.stringJvmTypeReference
			TemplateVariable: return typeComputer.getVariablesWithTypes(EcoreUtil2.getContainerOfType(variable, TemplateContainer)).get(variable).get // TODO
			default: throw new RuntimeException("Variable of type'"+variable.class.canonicalName+"' is unknown")
		}
	}
	
	val validCoercions = #{
		long.name->#[Long.name, long.name],
		Long.name->#[Long.name, long.name],
		boolean.name->#[Boolean.name, boolean.name],
		Boolean.name->#[Boolean.name, boolean.name],
		String.name->#[Long.name, long.name, Boolean.name, boolean.name]
	}

	def boolean coercibleTo(Expression expression, JvmTypeReference wantedType) {
		tclCoercionComputer.initWith(expression.eResource)
		val expressionType = determineType(expression, wantedType)
		return tclCoercionComputer.isCoercionPossible(wantedType, expressionType)
	}
	
	def JvmTypeReference moreSpecificType(JvmTypeReference left, JvmTypeReference right) {
		if(left.qualifiedName.equals(String.name)) {
			return right
		}else{
			return left
		}
	}
	
	// TODO: clean this goddamn hell up!
	def JvmTypeReference coercedTypeOfComparison(Comparison comparison, JvmTypeReference wantedType) {
		tclJvmTypeReferenceUtil.initWith(comparison.eResource)
		tclCoercionComputer.initWith(comparison.eResource)
		val leftType=comparison.left.determineType(wantedType)
		if (comparison.comparator === null) {
			return leftType
		}
		val rightType=comparison.right.determineType(null)
		switch (comparison.comparator) {
			ComparatorMatches: return tclJvmTypeReferenceUtil.stringJvmTypeReference// everything is coercible to string 
			ComparatorGreaterThan,
			ComparatorLessThan: if(coercibleTo(comparison.left, tclJvmTypeReferenceUtil.longObjectJvmTypeReference) && coercibleTo(comparison.right, tclJvmTypeReferenceUtil.longObjectJvmTypeReference)) {
				return tclJvmTypeReferenceUtil.longObjectJvmTypeReference
			} else {
				return null; // impossible
			}
			ComparatorEquals: {
				if (leftType.qualifiedName == rightType.qualifiedName) {
					return leftType
				}
				if (jsonUtil.isJsonType(leftType)) {
					return rightType // JsonType can be coerced to anything (e.g. asJsonPrimitive().asLong())
				}
				if (jsonUtil.isJsonType(rightType)) {
					return leftType // JsonType can be coerced to anything (e.g. asJsonPrimitive().asLong())
				} else {
					val leftCoercions = validCoercions.get(leftType.qualifiedName)
					val rightCoercions = validCoercions.get(rightType.qualifiedName)
					if (leftCoercions !== null && rightCoercions !== null) {
						if (!leftCoercions.findFirst [leftCoercibleType|
							rightCoercions.exists[leftCoercibleType.equals(it)]
						].empty) {
							return moreSpecificType(leftType, rightType)
						}
					}
				}
			}
			default: throw new RuntimeException('''Unknown comparision type «comparison.comparator»''')
		}
		throw new RuntimeException('''Unable to coerce a type in comparision.''')		
	}
	
}