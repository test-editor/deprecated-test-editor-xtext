package org.testeditor.tcl.dsl.jvmmodel

import org.testeditor.tml.AEComparison
import org.testeditor.tml.AENullCheck
import org.testeditor.tml.AEStringConstant
import org.testeditor.tml.AEVariableReference
import org.testeditor.tml.AssertionExpression
import org.testeditor.tml.Comparator
import org.testeditor.tml.ComparatorEquals
import org.testeditor.tml.ComparatorGreaterThen
import org.testeditor.tml.ComparatorLessThen
import org.testeditor.tml.ComparatorMatches

class TclAssertCallBuilder {
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

	def String build(AssertionExpression expression) {
		val assertionMethod = assertionMethod(expression)
		if (assertionMethod == null) {
			return '''// TODO no assertion method implementation for expression with type "«expression.class»"'''
		} else {
			return '''org.junit.Assert.«assertionMethod(expression)»(«expression.buildExpression»);'''
		}
	}

	private def AssertMethod assertionMethod(AssertionExpression expression) {
		return switch (expression) {
			AENullCheck: adjustedAssertMethod(AssertMethod.assertNotNull, expression.negated)
			AEVariableReference: AssertMethod.assertNotNull
			AEComparison: assertionMethod(expression.comparator)
			AEStringConstant: AssertMethod.assertNotNull
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

	private def dispatch String buildExpression(AssertionExpression expression) {
		throw new RuntimeException('''no builder found for type «expression.class»''')
	}

	private def dispatch String buildExpression(AENullCheck nullCheck) {
		return nullCheck.varReference.buildExpression
	}

	private def dispatch String buildExpression(AEComparison comparison) {
		if (comparison.comparator == null) {
			return comparison.left.
				buildExpression
		}
		switch (comparison.comparator) {
			ComparatorEquals: '''«comparison.left.buildExpression», «comparison.right.buildExpression»'''
			ComparatorGreaterThen: '''«comparison.left.buildExpression» «if(comparison.comparator.negated){'<='}else{'>'}» «comparison.right.buildExpression»'''
			ComparatorLessThen: '''«comparison.left.buildExpression» «if(comparison.comparator.negated){'>='}else{'<'}» «comparison.right.buildExpression»'''
			ComparatorMatches: '''«comparison.left.buildExpression».matches(«comparison.right.buildExpression»)'''
			default:
				throw new RuntimeException('''no builder found for comparator «comparison.comparator.class»''')
		}
	}

	private def dispatch String buildExpression(AEVariableReference varRef) {
		if (varRef.key == null) {
			return varRef.name
		} else {
			return '''«varRef.name».get("«varRef.key»")'''
		}
	}

	private def dispatch String buildExpression(AEStringConstant string) {
		return '''"«string.string»"'''
	}

}
