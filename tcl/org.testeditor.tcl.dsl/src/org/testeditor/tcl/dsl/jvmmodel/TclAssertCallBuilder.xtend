package org.testeditor.tcl.dsl.jvmmodel

import org.testeditor.tcl.Comparator
import org.testeditor.tcl.ComparatorEquals
import org.testeditor.tcl.ComparatorGreaterThen
import org.testeditor.tcl.ComparatorLessThen
import org.testeditor.tcl.ComparatorMatches

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

	private def String adjustedOrderComparator(Comparator comparator) {
		switch (comparator) {
			ComparatorLessThen: if (comparator.negated ?: false) {
				return '>='
			} else {
				return '<'
			}
			ComparatorGreaterThen: if (comparator.negated ?: false) {
				return '<='
			} else {
				return '>'
			}
			default: throw new RuntimeException('''comparator class «comparator.class» cannot produce order comparator''')
		}
	}

	public def String buildAssertCall(Boolean assertNegate, String variableName, Comparator comparator,
		String expressionString) {
		if (comparator ==
			null) {
			return '''org.junit.Assert.«adjustedAssertMethod(AssertMethod.assertNotNull, assertNegate)»(«variableName»);'''
		}

		val combinedNegate = (assertNegate ?: false).xor(comparator.negated ?:false)
		return switch (comparator) {
			ComparatorEquals: '''org.junit.Assert.«adjustedAssertMethod(AssertMethod.assertEquals, combinedNegate)»(«variableName», «expressionString»);'''
			ComparatorGreaterThen: '''// TODO not complete yet org.junit.Assert.«adjustedAssertMethod(AssertMethod.assertTrue, assertNegate)»(«variableName» «adjustedOrderComparator(comparator)» «expressionString»);'''
			ComparatorLessThen: '''// TODO not complete yet org.junit.Assert.«adjustedAssertMethod(AssertMethod.assertTrue, assertNegate)»(«variableName» «adjustedOrderComparator(comparator)» «expressionString»);'''
			ComparatorMatches: '''org.junit.Assert.«adjustedAssertMethod(AssertMethod.assertTrue, combinedNegate)»(«variableName».matches(«expressionString»));'''
			default: '''// TODO could not interpret comparator '«comparator»' '''
		}
	}

}
		