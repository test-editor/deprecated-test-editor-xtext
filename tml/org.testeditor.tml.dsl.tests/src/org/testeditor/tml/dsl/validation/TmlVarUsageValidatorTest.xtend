package org.testeditor.tml.dsl.validation

import java.util.Map
import javax.inject.Inject
import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.validation.ValidationMessageAcceptor
import org.junit.Before
import org.junit.Test
import org.mockito.ArgumentCaptor
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.aml.AmlFactory
import org.testeditor.tml.ComponentTestStepContext
import org.testeditor.tml.dsl.tests.TmlModelGenerator
import org.testeditor.tml.dsl.tests.parser.AbstractParserTest
import org.testeditor.tml.util.TmlModelUtil

import static org.mockito.Matchers.*

import static extension org.mockito.Mockito.*

class TmlVarUsageValidatorTest extends AbstractParserTest {

	@Mock TmlModelUtil tclModelUtil // injected into class under test
	@InjectMocks TmlValidator tclValidator // class under test
	@Mock JvmTypeReference typeRefMock
	@Mock ValidationMessageAcceptor messageAcceptor

	@Inject extension TmlModelGenerator

	val message = ArgumentCaptor.forClass(String)

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

		val state = tclValidator.setMessageAcceptor(messageAcceptor)
		state.state // needs to be called in order for internal state to be initialized. this again is necessary to allow messages to be issued on the "currentObject" of the validation
	}

	@Test
	def void warningForMultipleAssignments() {
		// given
		val testStepContext = componentTestStepContext(null) => [
			steps += testStepWithAssignment("variable", "some")
			steps += testStepWithAssignment("variable", "other")
		]
		val componentTestStepContext = testStepContext.assertInstanceOf(ComponentTestStepContext)

		when(typeRefMock.identifier).thenReturn(String.canonicalName)

		// when
		tclValidator.checkVariableUsageWithinAssertionExpressions(componentTestStepContext)

		// then
		messageAcceptor.verify.acceptWarning(message.capture, anyObject, anyObject, anyInt, anyString)
		message.value.assertMatches("Variable 'variable' is assigned more than once\\.")
	}

	@Test
	def void mapVariableUsage() {
		// given
		val testStepContext = componentTestStepContext(null) => [
			val assignment = testStepWithAssignment("variable", "some")
			steps += assignment
			steps += assertionTestStep => [
				expression = aeComparison => [
					left = aeVariableReference => [
						testStepWithAssignment = assignment
						key = "key"
					]
					comparator = comparatorEquals
					right = aeStringConstant => [string = "fixed value"]
				]
			]
		]
		val componentTestStepContext = testStepContext.assertInstanceOf(ComponentTestStepContext)

		when(typeRefMock.identifier).thenReturn(Map.canonicalName)

		// when
		tclValidator.checkVariableUsageWithinAssertionExpressions(componentTestStepContext)

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
				expression = aeComparison => [
					left = aeVariableReference => [testStepWithAssignment = assignment]
					comparator = comparatorEquals
					right = aeStringConstant => [string = "fixed value"]
				]
			]
		]
		val componentTestStepContext = testStepContext.assertInstanceOf(ComponentTestStepContext)

		when(typeRefMock.identifier).thenReturn(String.canonicalName)

		// when
		tclValidator.checkVariableUsageWithinAssertionExpressions(componentTestStepContext)

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
				expression = aeComparison => [
					left = aeVariableReference => [
						testStepWithAssignment = assignment
						key = "key"
					]
					comparator = comparatorEquals
					right = aeStringConstant => [string = "fixed value"]
				]
			]
		]
		val componentTestStepContext = testStepContext.assertInstanceOf(ComponentTestStepContext)

		when(typeRefMock.identifier).thenReturn(Integer.canonicalName)

		// when
		tclValidator.checkVariableUsageWithinAssertionExpressions(componentTestStepContext)

		// then
		messageAcceptor.verify.acceptError(message.capture, anyObject, anyObject, anyInt, anyString)
		message.value.assertMatches("Variable 'variable'.*does not implement.*")
	}

}
