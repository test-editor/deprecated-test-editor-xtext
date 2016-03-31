package org.testeditor.tcl.dsl.validation

import java.util.Map
import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.validation.ValidationMessageAcceptor
import org.junit.Before
import org.junit.Test
import org.mockito.ArgumentCaptor
import org.mockito.InjectMocks
import org.mockito.Mock
import org.mockito.MockitoAnnotations
import org.testeditor.aml.dsl.AmlStandaloneSetup
import org.testeditor.aml.impl.AmlFactoryImpl
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest
import org.testeditor.tcl.util.TclModelUtil

import static org.mockito.Matchers.*

import static extension org.mockito.Mockito.*

class TclValidatorTest extends AbstractParserTest {

	@Mock TclModelUtil tclModelUtil // injected into class under test
	@InjectMocks TclValidator tclValidator // class under test
	@Mock JvmTypeReference typeRefMock
	@Mock ValidationMessageAcceptor messageAcceptor

	val message = ArgumentCaptor.forClass(String)

	@Before
	override void setUp() {
		super.setUp
		MockitoAnnotations.initMocks(this)

		val injector = (new AmlStandaloneSetup).createInjectorAndDoEMFRegistration
		val amlFactory = injector.getInstance(AmlFactoryImpl)

		// always assume that the return type of the operation used in any step is of typeRefMock
		// => setting typeRefMock to return some type will affect all assignment steps! 
		val operationMock = mock(JvmOperation)
		when(tclModelUtil.getInteraction(anyObject)).thenReturn(amlFactory.createInteractionType => [
			defaultMethod = amlFactory.createMethodReference => [
				operation = operationMock
			]
		]);
		when(operationMock.getReturnType).thenReturn(typeRefMock)

		val state = tclValidator.setMessageAcceptor(messageAcceptor)
		state.state // needs to be called in order for internal state to be initialized. this again is necessary to allow messages to be issued on the "currentObject" of the validation
	}

	@Test
	def void errorForUnknownVar() {
		// given
		val testStepContext = parseTestStepContext('''
			Component: some_fantasy_component
			- assert unknownVar = "get some map"
		''')

		// when
		tclValidator.checkVariableUsageWithinAssertionExpressions(testStepContext)

		// then
		messageAcceptor.verify.acceptError(message.capture, anyObject, anyObject, anyInt, anyString)
		message.value.assertMatches("Variable 'unknownVar' is unknown.*")
	}

	@Test
	def void warningForMultipleAssignments() {
		// given
		val testStepContext = parseTestStepContext('''
			Component: some_fantasy_component
			- value = get some map
			- value = execute second assignment
		''')

		when(typeRefMock.identifier).thenReturn(String.canonicalName)

		// when
		tclValidator.checkVariableUsageWithinAssertionExpressions(testStepContext)

		// then
		messageAcceptor.verify.acceptWarning(message.capture, anyObject, anyObject, anyInt, anyString)
		message.value.assertMatches("Variable 'value' is assigned more than once\\.")
	}

	@Test
	def void mapVariableUsage() {
		// given
		val testStepContext = parseTestStepContext('''
			Component: some_fantasy_component
			- value = get some map
			- assert value.key == "fixed value"
		''')

		when(typeRefMock.identifier).thenReturn(Map.canonicalName)

		// when
		tclValidator.checkVariableUsageWithinAssertionExpressions(testStepContext)

		// then
		messageAcceptor.verify(never).acceptError(anyString, anyObject, anyObject, anyInt, anyString)
	}

	@Test
	def void variableUsage() {
		// given
		val testStepContext = parseTestStepContext('''
			Component: some_fantasy_component
			- value = get some map
			- assert value == "fixed value"
		''')

		when(typeRefMock.identifier).thenReturn(String.canonicalName)

		// when
		tclValidator.checkVariableUsageWithinAssertionExpressions(testStepContext)

		// then
		messageAcceptor.verify(never).acceptError(anyString, anyObject, anyObject, anyInt, anyString)
	}

	@Test
	def void illegalMapVariableUsage() {
		// given
		val testStepContext = parseTestStepContext('''
			Component: some_fantasy_component
			- value = get some that is not a map
			- assert value.key == "fixed value"
		''')

		when(typeRefMock.identifier).thenReturn(Integer.canonicalName)

		// when
		tclValidator.checkVariableUsageWithinAssertionExpressions(testStepContext)

		// then
		messageAcceptor.verify.acceptError(message.capture, anyObject, anyObject, anyInt, anyString)
		message.value.assertMatches("Variable 'value'.*does not implement.*")
	}

	private def TestStepContext parseTestStepContext(CharSequence seq) {
		return parse(testStepContextRule, seq, TestStepContext)
	}

}
