package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.eclipse.xtext.common.types.JvmType
import org.eclipse.xtext.common.types.JvmTypeReference
import org.junit.Test
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.fixture.core.TestRunReporter.Action
import org.testeditor.fixture.core.TestRunReporter.SemanticUnit
import org.testeditor.fixture.core.TestRunReporter.Status
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import org.testeditor.tcl.dsl.tests.TclModelGenerator

import static org.mockito.Matchers.*
import static org.mockito.Mockito.*

class TestRunReporterGeneratorTest extends AbstractTclTest {

	@InjectMocks TestRunReporterGenerator testRunReporterGenerator // class under test
	@Inject extension TclModelGenerator
	@Mock TclExpressionBuilder expressionBuilder

	@Test
	def void testBuildReporterCallToPassVariables() {
		// given
		val someJvmType = mock(JvmType)
		val jvmTypeReference = mock(JvmTypeReference)

		val foo = variableReference => [ variable = assignmentVariable("foo") ]
		val bar = variableReference => [ variable = assignmentVariable("bar") ]
		when(expressionBuilder.buildReadExpression(eq(foo), any())).thenReturn("foo")
		when(expressionBuilder.buildReadExpression(eq(bar), any())).thenReturn("bar.toString()")

		// when
		val resultingList = testRunReporterGenerator.buildReporterCall(someJvmType, SemanticUnit.COMPONENT, Action.ENTER,
			"message", "IDvar0", Status.STARTED, "reporter", #[foo, bar], jvmTypeReference); 
			

		// then
		resultingList => [
			get(0).assertEquals('''
			
			String IDvar0=newVarId(); reporter.enter('''.toString)
			get(1).assertEquals(someJvmType)
			get(2).assertEquals('''.COMPONENT, "message", IDvar0, TestRunReporter.Status.STARTED, variables("foo", foo, "bar", bar.toString()));'''.toString)
		]
	}
}
