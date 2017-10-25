package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.eclipse.xtext.common.types.JvmType
import org.junit.Test
import org.testeditor.fixture.core.TestRunReporter.SemanticUnit
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import org.testeditor.tcl.dsl.tests.TclModelGenerator

import static org.mockito.Mockito.*

class TestRunReporterGeneratorTest extends AbstractTclTest {

	@Inject TestRunReporterGenerator testRunReporterGenerator // class under test
	@Inject extension TclModelGenerator

	@Test
	def void testUnsplicingOfVariablePostfixString() {
		// given
		val someJvmType = mock(JvmType)

		val foo = variableReference => [ variable = assignmentVariable("foo") ]
		val bar = variableReference => [ variable = assignmentVariable("bar") ]

		// when
		val resultingList = testRunReporterGenerator.buildReporterEnterCall(someJvmType, SemanticUnit.COMPONENT,
			"message", "reporter", #[foo, bar]); 

		// then
		resultingList => [
			get(0).assertEquals('''
			
			reporter.enter('''.toString)
			get(1).assertEquals(someJvmType)
			get(2).assertEquals('''.COMPONENT, "message // foo = '" + foo + "', bar = '" + bar + "'");
			'''.toString)
		]
	}
}
