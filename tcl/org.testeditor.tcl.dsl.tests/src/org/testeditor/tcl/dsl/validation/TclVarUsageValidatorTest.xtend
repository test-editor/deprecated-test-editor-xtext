package org.testeditor.tcl.dsl.validation

import java.util.Map
import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.common.types.JvmTypeReference
import org.junit.Before
import org.junit.Test
import org.mockito.Mock
import org.testeditor.aml.AmlFactory
import org.testeditor.tcl.ComponentTestStepContext

import static org.mockito.Matchers.*

import static extension org.mockito.Mockito.*

class TclVarUsageValidatorTest extends AbstractTclValidatorTest {

	@Mock JvmTypeReference typeRefMock

	@Before
	def void initMocks() {
		val amlFactory = AmlFactory.eINSTANCE

		// always assume that the return type of the operation used in any step is of typeRefMock
		// => setting typeRefMock to return some type will affect all assignment steps!
		val operationMock = mock(JvmOperation)
		when(tclModelUtil.getInteraction(anyObject)).thenReturn(amlFactory.createInteractionType => [
			defaultMethod = amlFactory.createMethodReference => [
				operation = operationMock
			]
		])
		when(operationMock.getReturnType).thenReturn(typeRefMock)
	}

	@Test
	def void errorForMultipleAssignments() {
		// given
		val testStepContext = componentTestStepContext(null) => [
			steps += testStepWithAssignment("variable", "some")
			steps += testStepWithAssignment("variable", "other")
		]
		val componentTestStepContext = testStepContext.assertInstanceOf(ComponentTestStepContext)

		when(typeRefMock.identifier).thenReturn(String.canonicalName)

		// when
		tclValidator.executeCheckVariableUsageWithinAssertionExpressions(componentTestStepContext, newHashMap)

		// then
		messageAcceptor.verify.acceptError(message.capture, anyObject, anyObject, anyInt, anyString)
		message.value.assertMatches("Variable 'variable' is assigned more than once\\.")
	}

	@Test
	def void mapVariableUsage() {
		// given
		val testStepContext = componentTestStepContext(null) => [
			val assignment = testStepWithAssignment("variable", "some")
			steps += assignment
			steps += assertionTestStep => [
				expression = assignment.variable.mappedReference.compareOnEquality("fixed value")
			]
		]
		val componentTestStepContext = testStepContext.assertInstanceOf(ComponentTestStepContext)

		when(typeRefMock.identifier).thenReturn(Map.canonicalName)

		// when
		tclValidator.executeCheckVariableUsageWithinAssertionExpressions(componentTestStepContext, newHashMap)

		// then
		messageAcceptor.verify(never).acceptError(anyString, anyObject, anyObject, anyInt, anyString)
	}

	@Test
	def void variableUsage() {
		// given
		val testStepContext = componentTestStepContext(null) => [
			val assignment = testStepWithAssignment("variable", "some")
			steps += assignment
			steps += assertionTestStep => [
				expression = assignment.variable.flatReference.compareOnEquality("fixed value")
			]
		]
		val componentTestStepContext = testStepContext.assertInstanceOf(ComponentTestStepContext)

		when(typeRefMock.identifier).thenReturn(String.canonicalName)

		// when
		tclValidator.executeCheckVariableUsageWithinAssertionExpressions(componentTestStepContext, newHashMap)

		// then
		messageAcceptor.verify(never).acceptError(anyString, anyObject, anyObject, anyInt, anyString)
	}

	@Test
	def void usageFromOtherContextSameMacro() {
		// given
		val assignment = testStepWithAssignment("variable", "some")
		val macro = macro("some") => [
			contexts += componentTestStepContext(null) => [
				steps += assignment
			]
			contexts += componentTestStepContext(null) => [
				steps += assertionTestStep => [
					expression = assignment.variable.flatReference.compareOnEquality("fixed value")
				]
			]
		]

		// when
		tclValidator.checkVariableUsageWithinAssertionExpressions(macro)

		// then
		messageAcceptor.verify(never).acceptError(anyString, anyObject, anyObject, anyInt, anyString)
	}

	@Test
	def void illegalMapVariableUsage() {
		// given
		val testStepContext = componentTestStepContext(null) => [
			val assignment = testStepWithAssignment("variable", "some")
			steps += assignment
			steps += assertionTestStep => [
				expression = assignment.variable.mappedReference.compareOnEquality("fixed value")
			]
		]
		val componentTestStepContext = testStepContext.assertInstanceOf(ComponentTestStepContext)

		when(typeRefMock.identifier).thenReturn(Integer.canonicalName)

		// when
		tclValidator.executeCheckVariableUsageWithinAssertionExpressions(componentTestStepContext, newHashMap)

		// then
		messageAcceptor.verify.acceptError(message.capture, anyObject, anyObject, anyInt, anyString)
		message.value.assertMatches("Variable 'variable'.*does not implement.*")
	}

	@Test
	def void usageFromOtherContextSameTestCase() {
		// given
		val assignment = testStepWithAssignment("variable", "some")
		val tclModel = tclModel => [
			test = testCase => [
				steps += specificationStep("first") => [
					contexts += componentTestStepContext(null) => [
						steps += assignment
					]
				]
				steps += specificationStep("second") => [
					contexts += componentTestStepContext(null) => [
						steps += assertionTestStep => [
							expression = assignment.variable.flatReference.compareOnEquality("fixed value")
						]
					]
				]
			]
		]

		// when
		tclValidator.checkVariableUsageWithinAssertionExpressions(tclModel)

		// then
		messageAcceptor.verify(never).acceptError(anyString, anyObject, anyObject, anyInt, anyString)
	}

}

