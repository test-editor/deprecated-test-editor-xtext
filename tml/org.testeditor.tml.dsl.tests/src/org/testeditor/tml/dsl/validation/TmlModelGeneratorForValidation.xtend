package org.testeditor.tml.dsl.validation

import org.testeditor.tml.AEComparison
import org.testeditor.tml.AEVariableReference
import org.testeditor.tml.AssignmentVariable
import org.testeditor.tml.dsl.tests.TmlModelGenerator
import javax.inject.Inject

class TmlModelGeneratorForValidation {
	
	@Inject extension TmlModelGenerator

	def AEComparison compareOnEquality(AEVariableReference variableReference, String string) {
		return aeComparison => [
			left = variableReference
			comparator = comparatorEquals
			right = aeStringConstant(string)
		]
	}

	def AEVariableReference flatReference(AssignmentVariable assignmentVariable) {
		return aeVariableReference => [
			variable = assignmentVariable
		]
	}

	def AEVariableReference mappedReference(AssignmentVariable assignmentVariable) {
		return aeVariableReference => [
			variable = assignmentVariable
			key = "key"
		]
	}

}
