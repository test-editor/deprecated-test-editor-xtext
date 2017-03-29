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

import com.google.inject.Inject
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.xbase.jvmmodel.JvmTypeReferenceBuilder
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
import org.testeditor.tcl.NullOrBoolCheck
import org.testeditor.tcl.StringConstant
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.dsl.validation.TclTypeValidationUtil
import org.testeditor.tcl.VariableReferenceMapAccess

class TclExpressionTypeComputer {
	@Inject SimpleTypeComputer typeComputer
	@Inject TclTypeValidationUtil typeValidationUtil
	@Inject JvmTypeReferenceBuilder.Factory typeReferenceBuilderFactory
	@Inject ResourceSet resourceSet
	
	var JvmTypeReferenceBuilder typeReferenceBuilder
	
	def JvmTypeReference determineType(Expression expression) {
		switch (expression) {
			VariableReferenceMapAccess: return String.buildFrom
			VariableReference: return expression.variable.determineType
			StringConstant: return String.buildFrom
			Comparison: if(expression.comparator === null) {
				expression.left.determineType
			} else {
				return boolean.buildFrom
			}
			NullOrBoolCheck: return boolean.buildFrom
			default: { throw new RuntimeException("Expression of type '"+expression.class.canonicalName+"' is unknown")}
		}
	}
	
	def JvmTypeReference determineType(Variable variable) {
		switch (variable) {
			AssignmentVariable : return typeValidationUtil.determineType(variable)
			EnvironmentVariable : return String.buildFrom
			TemplateVariable: return typeComputer.getVariablesWithTypes(EcoreUtil2.getContainerOfType(variable, TemplateContainer)).get(variable).get // TODO
			default: throw new RuntimeException("Variable of type'"+variable.class.canonicalName+"' is unknown")
		}
	}
	
	private def JvmTypeReference buildFrom(Class<?> clazz){
		if (typeReferenceBuilder===null) {
			typeReferenceBuilder = typeReferenceBuilderFactory.create(resourceSet)
		}
		return typeReferenceBuilder.typeRef(clazz)
	}
	
	val validCoercions = #{
		long.name->#[Long.name, long.name],
		Long.name->#[Long.name, long.name],
		boolean.name->#[Boolean.name, boolean.name],
		Boolean.name->#[Boolean.name, boolean.name],
		String.name->#[Long.name, long.name, Boolean.name, boolean.name]
	}
	
	def boolean coercibleTo(Expression expression, JvmTypeReference wantedType) {
		if (expression instanceof Comparison) {
			if (expression.comparator === null) {
				return coercibleTo(expression.left, wantedType)
			}
		}else if (expression instanceof StringConstant) {
			switch (wantedType.qualifiedName) {
				case long.name,
				case Long.name:
					try {
						Long.parseLong(expression.string)
						return true
					} catch (NumberFormatException nfe) {
						return false
					}
				case boolean.name,
				case Boolean.name:
					return expression.string.equals(Boolean.TRUE.toString) ||
						expression.string.equals(Boolean.FALSE.toString)
			}
		}
		val type=expression.determineType
		val validCoercion=validCoercions.get(type.qualifiedName)
		return (validCoercion!==null && validCoercion.toList.contains(wantedType.qualifiedName))
	}
	
	def JvmTypeReference moreSpecificType(JvmTypeReference left, JvmTypeReference right) {
		if(left.qualifiedName.equals(String.name)) {
			return right
		}else{
			return left
		}
	}
	
	def JvmTypeReference coercedTypeOfComparison(Comparison comparison) {
		val leftType=comparison.left.determineType
		if(comparison.comparator===null) {
			return leftType
		}
		val rightType=comparison.right.determineType
		switch (comparison.comparator) {
			ComparatorMatches: return String.buildFrom // everything is coercible to string 
			ComparatorGreaterThan,
			ComparatorLessThan: if(coercibleTo(comparison.left, Long.buildFrom) && coercibleTo(comparison.right,Long.buildFrom)) {
				return Long.buildFrom
			} else {
				return null; // impossible
			}
			ComparatorEquals: {
				if (leftType.qualifiedName == rightType.qualifiedName) {
					return leftType
				} else {
					val leftCoercions=validCoercions.get(leftType.qualifiedName)
					val rightCoercions=validCoercions.get(rightType.qualifiedName)
					if(leftCoercions!==null && rightCoercions!==null) {
						if(!leftCoercions.findFirst[leftCoercibleType|rightCoercions.exists[leftCoercibleType.equals(it)]].empty) {
							return moreSpecificType(leftType,rightType)
						}
					}
				}
			}
			default: throw new RuntimeException('''Unknown comparision type «comparison.comparator»''')
		}
		return null
		
	}
	
}