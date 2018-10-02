package org.testeditor.tcl.dsl.jvmmodel

import java.util.Optional
import javax.inject.Inject
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.common.types.JvmType
import org.eclipse.xtext.common.types.JvmTypeReference
import org.junit.Test
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.dsl.common.util.JvmTypeReferenceUtil
import org.testeditor.fixture.core.MaskingString
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
	@Mock TclGeneratorConfig generatorConfig
	@Mock TclExpressionTypeComputer typeComputer
	@Inject JvmTypeReferenceUtil typeReferenceUtil
	
	@Test
	def void testCommentPrefix() {
		val someJvmType = mock(JvmType)
		val jvmTypeReference = mock(JvmTypeReference)
		when(generatorConfig.reporterCallCommentPrefixChar).thenReturn(' ')
		when(generatorConfig.reporterCallCommentPrefixCount).thenReturn(10)

		// when
		val resultingList = testRunReporterGenerator.buildReporterCall(someJvmType, SemanticUnit.COMPONENT, Action.ENTER,
			"message", "IDvar0", Status.STARTED, "reporter", #[], jvmTypeReference); 

		// then
		resultingList => [
			get(0).assertEquals('''
			
			/*          */ String IDvar0=newVarId(); reporter.enter('''.toString)			
		]
	}

	@Test
	def void testBuildReporterCallToPassVariables() {
		// given
		val someJvmType = mock(JvmType)
		val jvmTypeReference = mock(JvmTypeReference)

		val foo = variableReference => [ variable = assignmentVariable("foo") ]
		val bar = variableReference => [ variable = assignmentVariable("bar") ]
		when(expressionBuilder.buildReadExpression(eq(foo), any())).thenReturn("foo")
		when(expressionBuilder.buildReadExpression(eq(bar), any())).thenReturn("bar.toString()")
		typeReferenceUtil.initWith(null as ResourceSet)
		when(typeComputer.determineType(foo.variable, Optional.empty)).thenReturn(typeReferenceUtil.buildFrom(MaskingString))
		when(typeComputer.determineType(bar.variable, Optional.empty)).thenReturn(typeReferenceUtil.buildFrom(String))

		// when
		val resultingList = testRunReporterGenerator.buildReporterCall(someJvmType, SemanticUnit.COMPONENT, Action.ENTER,
			"message", "IDvar0", Status.STARTED, "reporter", #[foo, bar], jvmTypeReference); 
			

		// then
		resultingList => [
			get(0).assertEquals('''
			
			String IDvar0=newVarId(); reporter.enter('''.toString)
			get(1).assertEquals(someJvmType)
			get(2).assertEquals('''.COMPONENT, "message", IDvar0, TestRunReporter.Status.STARTED, variables("foo", "*****", "bar", bar.toString()));'''.toString)
		]
	}
}
