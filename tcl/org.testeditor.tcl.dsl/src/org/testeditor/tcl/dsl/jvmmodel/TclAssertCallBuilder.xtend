package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.testeditor.aml.ModelUtil
import org.testeditor.tcl.Comparator
import org.testeditor.tcl.ComparatorEquals
import org.testeditor.tcl.ComparatorGreaterThen
import org.testeditor.tcl.ComparatorLessThen
import org.testeditor.tcl.ComparatorMatches
import org.testeditor.tcl.Expression
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.VariableReferenceMapAccess
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tcl.NullOrBoolCheck
import org.testeditor.tcl.Comparison
import org.testeditor.tcl.StringConstant

class TclAssertCallBuilder {
	@Inject extension TclModelUtil
	@Inject extension ModelUtil

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
			val assertionText = NodeModelUtils.getNode(expression)?.text?.trim ?:
				""
			return '''
				// - assert «assertionText»
				org.junit.Assert.«expression.assertionMethod»("«assertionText.replaceAll('"','\\\\"')»", «expression.buildExpression»);
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
			ComparatorGreaterThen: null // TODO adjustedAssertMethod(AssertMethod.assertTrue, comparator.negated)
			ComparatorLessThen: null // TODO adjustedAssertMethod(AssertMethod.assertTrue, comparator.negated)
			ComparatorMatches: adjustedAssertMethod(AssertMethod.assertTrue, comparator.negated)
			default: throw new RuntimeException('''unknown comparator type «comparator.class»''')
		}

	}

	private def dispatch String buildExpression(Expression expression) {
		throw new RuntimeException('''no builder found for type «expression.class»''')
	}

	private def dispatch String buildExpression(NullOrBoolCheck nullCheck) {
		val expression = nullCheck.variableReference.buildExpression
		val interaction = nullCheck.testStep.interaction
		val returnType = interaction.returnType
		if (Boolean.isAssignableWithoutConversion(returnType)) {
			return '''(«expression» != null) && «expression».booleanValue()'''
		} else {
			return expression
		}
	}

	private def dispatch String buildExpression(Comparison comparison) {
		if (comparison.comparator == null) {
			return comparison.left.
				buildExpression
		}
		switch (comparison.comparator) {
			ComparatorEquals: '''«comparison.right.buildExpression», «comparison.left.buildExpression»'''
			ComparatorGreaterThen: '''«comparison.left.buildExpression» «if(comparison.comparator.negated){'<='}else{'>'}» «comparison.right.buildExpression»'''
			ComparatorLessThen: '''«comparison.left.buildExpression» «if(comparison.comparator.negated){'>='}else{'<'}» «comparison.right.buildExpression»'''
			ComparatorMatches: '''«comparison.left.buildExpression».matches(«comparison.right.buildExpression»)'''
			default:
				throw new RuntimeException('''no builder found for comparator «comparison.comparator.class»''')
		}
	}

	private def dispatch String buildExpression(VariableReferenceMapAccess varRef) {
		return '''«varRef.variable.name».get("«varRef.key»")'''
	}
	
	private def dispatch String buildExpression(VariableReference varRef) {
		return varRef.variable.name
	}

	private def dispatch String buildExpression(StringConstant string) {
		return '''"«string.string»"'''
	}

}
