package org.testeditor.tcl.dsl.validation

import javax.inject.Inject
import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.validation.ValidationMessageAcceptor
import org.junit.Before
import org.junit.Test
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.aml.AmlFactory
import org.testeditor.tcl.dsl.tests.TclModelGenerator
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest
import org.testeditor.tcl.util.TclModelUtil

import static org.mockito.Matchers.*

import static extension org.mockito.Mockito.*

class TclVarUsageValidatorTest extends AbstractParserTest {

	@InjectMocks TclValidator tclValidator // class under test
	@Mock TclModelUtil tclModelUtil // injected into class under test
	@Mock JvmTypeReference typeRefMock
	@Mock ValidationMessageAcceptor messageAcceptor

	@Inject extension TclModelGenerator

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
	def void usageFromOtherContext() {
		// given
		val assignment = testStepWithAssignment("variable", "some")
		val tclModel = tclModel => [
			test = testCase("testcase") => [
				steps += specificationStep("first") => [
					contexts += componentTestStepContext(null) => [
						steps += assignment
					]
				]
				steps += specificationStep("second") => [
					contexts += componentTestStepContext(null) => [
						steps += assertionTestStep => [
							expression = assignment.assignmentVariable.flatReference.compareOnEquality("fixed value")
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
