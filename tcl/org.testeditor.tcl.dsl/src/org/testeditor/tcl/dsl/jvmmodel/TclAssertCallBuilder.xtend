package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.apache.commons.lang3.StringEscapeUtils
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.testeditor.aml.ModelUtil
import org.testeditor.tcl.Comparator
import org.testeditor.tcl.ComparatorEquals
import org.testeditor.tcl.ComparatorGreaterThen
import org.testeditor.tcl.ComparatorLessThen
import org.testeditor.tcl.ComparatorMatches
import org.testeditor.tcl.Comparison
import org.testeditor.tcl.Expression
import org.testeditor.tcl.NullOrBoolCheck
import org.testeditor.tcl.StringConstant
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.util.TclModelUtil

class TclAssertCallBuilder {
	@Inject extension TclModelUtil
	@Inject extension ModelUtil
	
	@Inject TclExpressionBuilder expressionBuilder

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

	def String build(Expression expression) {
		val assertionMethod = assertionMethod(expression)
		if (assertionMethod == null) {
			return '''// TODO no assertion method implementation for expression with type "«expression.class»"'''
		} else {
			val assertionText = NodeModelUtils.getNode(expression)?.text?.trim ?: ""
			val expressionBuilt = switch (expression) {
				NullOrBoolCheck: buildNullOrBoolCheck(expression) 
				Comparison: buildComparison(expression) 
				default: throw new RuntimeException('''Assertion expression of type='«expression.class.canonicalName»' cannot be built!''')
			}
			return '''
				// - assert «assertionText»
				org.junit.Assert.«expression.assertionMethod»("«StringEscapeUtils.escapeJava(assertionText)»", «expressionBuilt»);
			'''
		}
	}

	private def AssertMethod assertionMethodForNullOrBoolCheck(NullOrBoolCheck expression) {
		val interaction = expression.testStep.interaction
		val returnTypeName = interaction.returnType?.qualifiedName ?: ""
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
			StringConstant: AssertMethod.assertNotNull
			default: throw new RuntimeException('''unknown expression type «expression.class»''')
		}
	}

	private def AssertMethod assertionMethod(Comparator comparator) {
		if (comparator == null) {
			return AssertMethod.assertNotNull
		}
		return switch (comparator) {
			ComparatorEquals: adjustedAssertMethod(AssertMethod.assertEquals, comparator.negated)
			ComparatorGreaterThen: throw new RuntimeException('>= not implemented yet') // adjustedAssertMethod(AssertMethod.assertTrue, comparator.negated)
			ComparatorLessThen: throw new RuntimeException('<= not implemented yet') // adjustedAssertMethod(AssertMethod.assertTrue, comparator.negated)
			ComparatorMatches: adjustedAssertMethod(AssertMethod.assertTrue, comparator.negated)
			default: throw new RuntimeException('''unknown comparator type «comparator.class»''')
		}

	}

	/**
	 * return a string that is directly usable within an assertion command
	 */
	def String buildComparison(Comparison comparison) {
		if (comparison.comparator == null) {
			return expressionBuilder.buildExpression(comparison.left)
		}
		val builtRightExpression=expressionBuilder.buildExpression(comparison.right)
		val builtLeftExpression=expressionBuilder.buildExpression(comparison.left)
		switch (comparison.comparator) {
			ComparatorEquals: '''«builtRightExpression», «builtLeftExpression»'''
			ComparatorGreaterThen: '''«builtLeftExpression» «if(comparison.comparator.negated){'<='}else{'>'}» «builtRightExpression»'''
			ComparatorLessThen: '''«builtLeftExpression» «if(comparison.comparator.negated){'>='}else{'<'}» «builtRightExpression»'''
			ComparatorMatches: '''«builtLeftExpression».toString().matches(«builtRightExpression».toString())'''
			default:
				throw new RuntimeException('''no builder found for comparator «comparison.comparator.class»''')
		}
	}

	/**
	 * return a string that is directly usable within an assertion command
	 */
	private def String buildNullOrBoolCheck(NullOrBoolCheck nullCheck) {
		val builtExpression = expressionBuilder.buildExpression(nullCheck.variableReference)
		val interaction = nullCheck.testStep.interaction
		val returnType = interaction.returnType
		if (Boolean.isAssignableWithoutConversion(returnType)) {
			return '''(«builtExpression» != null) && «builtExpression».booleanValue()'''
		} else {
			return builtExpression
		}
	}

}
