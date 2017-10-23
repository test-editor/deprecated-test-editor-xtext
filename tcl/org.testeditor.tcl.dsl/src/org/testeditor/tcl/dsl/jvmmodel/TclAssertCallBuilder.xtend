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
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.slf4j.LoggerFactory
import org.testeditor.tcl.Comparator
import org.testeditor.tcl.ComparatorEquals
import org.testeditor.tcl.ComparatorGreaterThan
import org.testeditor.tcl.ComparatorLessThan
import org.testeditor.tcl.ComparatorMatches
import org.testeditor.tcl.Comparison
import org.testeditor.tcl.Expression
import org.testeditor.tcl.JsonNumber
import org.testeditor.tcl.JsonString
import org.testeditor.tcl.NullOrBoolCheck
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.VariableReference

import static extension org.apache.commons.lang3.StringEscapeUtils.escapeJava
import static extension org.eclipse.xtext.EcoreUtil2.getContainerOfType
import org.testeditor.dsl.common.util.JvmTypeReferenceUtil

/**
 * generate code used for assertion statements
 */
class TclAssertCallBuilder {

	static val logger = LoggerFactory.getLogger(TclAssertCallBuilder)

	@Inject extension VariableCollector	
	@Inject TclExpressionBuilder expressionBuilder
	@Inject JvmTypeReferenceUtil typeReferenceUtil
	@Inject TclExpressionTypeComputer expressionTypeComputer

	/** assert method calls used, toString must yield the actual method name! */
	enum AssertMethod {
		assertNull,
		assertNotNull,
		assertEquals,
		assertNotEquals,
		assertTrue,
		assertFalse
	}

	static val ASSERT_METHOD_NEGATION = #{
		AssertMethod.assertNull -> AssertMethod.assertNotNull,
		AssertMethod.assertNotNull -> AssertMethod.assertNull,
		AssertMethod.assertTrue -> AssertMethod.assertFalse,
		AssertMethod.assertFalse -> AssertMethod.assertTrue,
		AssertMethod.assertEquals -> AssertMethod.assertNotEquals,
		AssertMethod.assertNotEquals -> AssertMethod.assertEquals
	}

	private def AssertMethod adjustedAssertMethod(AssertMethod method, Boolean negate) {
		return if (negate ?: false) {
			ASSERT_METHOD_NEGATION.get(method)
		} else {
			method
		}
	}
	
	def String assertionText(Expression expression) {
		return NodeModelUtils.getNode(expression)?.text?.trim ?: ""
	}

	def String build(Expression expression, String messagePrefix) {
		val assertionMethod = assertionMethod(expression)
		if (assertionMethod == null) {
			return '''// TODO no assertion method implementation for expression with type "«expression.class»"'''
		} else {
			val expressionBuilt = switch (expression) {
				NullOrBoolCheck: buildNullOrBoolCheck(expression) 
				Comparison: buildComparison(expression) 
				default: throw new RuntimeException('''Assertion expression of type='«expression.class.canonicalName»' cannot be built!''')
			}
			return '''
				org.junit.Assert.«expression.assertionMethod»("«messagePrefix»: «expression.assertionText.escapeJava»", «expressionBuilt»);'''
		}
	}

	private def AssertMethod assertionMethodForNullOrBoolCheck(NullOrBoolCheck expression) {
		val variableTypeMap = expression.getContainerOfType(TestStepContext).collectDeclaredVariablesTypeMap
		val returnTypeName = variableTypeMap.get(expression.variableReference.variable.name).qualifiedName
		logger.trace(
			"determines assertion method based on return type name='{}' for null or bool check of variable='{}'",
			returnTypeName, expression.variableReference.variable.name)
		switch (returnTypeName) {
			case boolean.name,
			case Boolean.name: return adjustedAssertMethod(AssertMethod.assertTrue, expression.isNegated)
			default: return adjustedAssertMethod(AssertMethod.assertNotNull, expression.isNegated)
		}
	}

	private def AssertMethod assertionMethod(Expression expression) {
		return switch (expression) {
			NullOrBoolCheck: assertionMethodForNullOrBoolCheck(expression)
			VariableReference: AssertMethod.assertNotNull
			Comparison: assertionMethod(expression.comparator)
			JsonString: AssertMethod.assertNotNull
			JsonNumber: AssertMethod.assertNotNull
			default: throw new RuntimeException('''unknown expression type «expression.class»''')
		}
	}

	private def AssertMethod assertionMethod(Comparator comparator) {
		if (comparator == null) {
			return AssertMethod.assertNotNull
		}
		return switch (comparator) {
			ComparatorEquals: adjustedAssertMethod(AssertMethod.assertEquals, comparator.negated)
			ComparatorGreaterThan: adjustedAssertMethod(AssertMethod.assertTrue, comparator.negated)
			ComparatorLessThan: adjustedAssertMethod(AssertMethod.assertTrue, comparator.negated)
			ComparatorMatches: adjustedAssertMethod(AssertMethod.assertTrue, comparator.negated)
			default: throw new RuntimeException('''unknown comparator type «comparator.class»''')
		}

	}
	
	/**
	 * return a string that is directly usable within an assertion command
	 */
	private def String buildComparison(Comparison comparison) {
		if (comparison.comparator == null) {
			return expressionBuilder.buildReadExpression(comparison.left)
		}
		val wantedType = expressionTypeComputer.coercedTypeOfComparison(comparison, Optional.empty)
		val builtRightExpression=expressionBuilder.buildReadExpression(comparison.right, wantedType)
		val builtLeftExpression=expressionBuilder.buildReadExpression(comparison.left, wantedType)
		typeReferenceUtil.initWith(comparison.eResource)
		switch (comparison.comparator) {
			ComparatorEquals: return '''«builtRightExpression», «builtLeftExpression»'''
			ComparatorGreaterThan:
				if (typeReferenceUtil.isBigDecimal(wantedType)) {
					return '''«builtLeftExpression».compareTo(«builtRightExpression») «if(comparison.comparator.negated){'<='}else{'>'}» 0'''
				}else{
					return '''«builtLeftExpression» «if(comparison.comparator.negated){'<='}else{'>'}» «builtRightExpression»'''
				}
			ComparatorLessThan:
				if (typeReferenceUtil.isBigDecimal(wantedType)) {
					return '''«builtLeftExpression».compareTo(«builtRightExpression») «if(comparison.comparator.negated){'>='}else{'<'}» 0'''
				} else {
					return '''«builtLeftExpression» «if(comparison.comparator.negated){'>='}else{'<'}» «builtRightExpression»'''
				}
			ComparatorMatches: return '''«builtLeftExpression».toString().matches(«builtRightExpression».toString())'''
			default:
				throw new RuntimeException('''no builder found for comparator «comparison.comparator.class»''')
		}
	}
	
	/**
	 * return a string that is directly usable within an assertion command
	 */
	private def String buildNullOrBoolCheck(NullOrBoolCheck nullCheck) {
		typeReferenceUtil.initWith(nullCheck.eResource)
		val builtExpression = expressionBuilder.buildReadExpression(nullCheck.variableReference)
		val variableTypeMap = nullCheck.getContainerOfType(TestStepContext).collectDeclaredVariablesTypeMap
		val returnType = variableTypeMap.get(nullCheck.variableReference.variable.name)
		logger.trace("builds expression based on return type name='{}' for null or bool check of variable='{}'",
			returnType.qualifiedName, nullCheck.variableReference.variable.name)
		if (returnType.isABooleanObjectType) {
			return '''(«builtExpression» != null) && «builtExpression».booleanValue()'''
		} else {
			return builtExpression
		}
	}
	
	/** 
	 * Is the given typeReference of Boolean type and an Object (no primitive type)?
	 */
	private def boolean isABooleanObjectType(JvmTypeReference typeReference) {
		return typeReferenceUtil.isAssignableFrom(typeReferenceUtil.booleanObjectJvmTypeReference,
			typeReference, typeReferenceUtil.checkWithoutBoxing)
	}

}
