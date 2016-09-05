package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.junit.Test
import org.testeditor.aml.TemplateVariable
import org.testeditor.aml.dsl.tests.AmlModelGenerator
import org.testeditor.tcl.AssignmentVariable
import org.testeditor.tcl.EnvironmentVariable
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.dsl.tests.TclModelGenerator
import org.testeditor.tcl.dsl.validation.AbstractUnmockedTclValidatorTest
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.aml.Variable

class MacroCallVariableResolverTest extends AbstractUnmockedTclValidatorTest {
	@Inject extension TclModelGenerator
	@Inject extension AmlModelGenerator

	@Inject MacroCallVariableResolver classUnderTest

	@Test
	def void testResolvingReferenceToAssignmentVariable() {
		// given
		classUnderTest.macroUseStack = #[]
		val variable = variableReference => [variable = assignmentVariable("some")]

		// when
		val resolvedVariable = classUnderTest.resolveVariableReference(variable)

		// then
		resolvedVariable.assertInstanceOf(VariableReference) //
		.variable.assertInstanceOf(AssignmentVariable).name.assertEquals("some")
	}

	@Test
	def void testResolvingReferenceToEnvironmentVariable() {
		// given
		classUnderTest.macroUseStack = #[]
		val variable = variableReference => [variable = environmentVariables("some").head]

		// when
		val resolvedVariable = classUnderTest.resolveVariableReference(variable)

		// then
		resolvedVariable.assertInstanceOf(VariableReference) //
		.variable.assertInstanceOf(EnvironmentVariable).name.assertEquals("some")
	}

	@Test
	def void testResolvingReferenceToEnvironmentVariableThroughSingleMacroCall() {
		// given
		val environmentVariable = environmentVariables("some").head
		val macroParameter = parameterForCallingSingleMacro(environmentVariable)
		val macroParameterReference = variableReference => [variable = macroParameter] 

		// when
		val resolvedVariable = classUnderTest.resolveVariableReference(macroParameterReference)

		// then
		resolvedVariable.assertInstanceOf(VariableReference) //
		.variable.assertInstanceOf(EnvironmentVariable).name.assertEquals("some")
	}

	@Test
	def void testResolvingReferenceToEnvironmentVariableThroughChainedMacroCall() {
		// given
		val environmentVariable = environmentVariables("some").head
		val macroParameter = macroParameterAtEndOfCallChain(environmentVariable)
		val macroParameterReference = variableReference => [variable = macroParameter] 
		
		// when
		val resolvedVariable = classUnderTest.resolveVariableReference(macroParameterReference)

		// then
		resolvedVariable.assertInstanceOf(VariableReference) //
		.variable.assertInstanceOf(EnvironmentVariable).name.assertEquals(environmentVariable.name)
	}
	
	@Test
	def void testResolvingReferenceToAssignmentVariableThroughSingleMacroCall() {
		// given
		val environmentVariable = assignmentVariable("some")
		val macroParameter = parameterForCallingSingleMacro(environmentVariable)
		val macroParameterReference = variableReference => [variable = macroParameter] 

		// when
		val resolvedVariable = classUnderTest.resolveVariableReference(macroParameterReference)

		// then
		resolvedVariable.assertInstanceOf(VariableReference) //
		.variable.assertInstanceOf(AssignmentVariable).name.assertEquals("some")
	}

	@Test
	def void testResolvingReferenceToAssignmentVariableThroughChainedMacroCall() {
		// given
		val environmentVariable = assignmentVariable("some")
		val macroParameter = macroParameterAtEndOfCallChain(environmentVariable)
		val macroParameterReference = variableReference => [variable = macroParameter] 
		
		// when
		val resolvedVariable = classUnderTest.resolveVariableReference(macroParameterReference)

		// then
		resolvedVariable.assertInstanceOf(VariableReference) //
		.variable.assertInstanceOf(AssignmentVariable).name.assertEquals(environmentVariable.name)
	}
	
	/** build a context where a regular test steps calls a macro 
	 *  => result is the macro parameter name 
	 * 
	 *  the (macro) call stack is passed to the class under test
	 */
	private def TemplateVariable parameterForCallingSingleMacro(Variable variable) {
		val macroCollection = macroCollection("MacroCollection") => [
			macros += macro("MyCallMacro") => [
				template = template("mycall").withParameter("appname")
				val parameter = template.contents.last as TemplateVariable
				contexts += componentTestStepContext(dummyComponent) => [
					steps += testStep("start").withReferenceToVariable(parameter)
				]
			]
		]

		val result = macroCollection.macros.head.template.contents.last as TemplateVariable
		val macroTestStepContext = macroTestStepContext(macroCollection) => [
			step = testStep("mycall").withReferenceToVariable(variable)
		]

		classUnderTest.macroUseStack = #[macroTestStepContext]
		
		return result

	}
	
	/** build a context where a regular test steps calls a macro which in turn calls another macro 
	 *  => result is the macro parameter name of the last macro called
	 * 
	 *  the (macro) call stack is passed to the class under test
	 */
	private def TemplateVariable macroParameterAtEndOfCallChain(Variable variable) {
		val macroCollection = macroCollection("MacroCollection")
		macroCollection => [
			macros += macro("MyCallMacro") => [
				template = template("mycall").withParameter("appname")
				val parameter = template.contents.last as TemplateVariable
				contexts += componentTestStepContext(dummyComponent) => [
					steps += testStep("start").withReferenceToVariable(parameter)
				]
			]
			macros += macro("MyOtherCallMacro") => [
				template = template("myothercall").withParameter("otherparam")
				val parameter = template.contents.last as TemplateVariable
				contexts += macroTestStepContext(macroCollection) => [
					step = testStep("mycall").withReferenceToVariable(parameter)
				]
			]
		]

		val result = macroCollection.macros.head.template.contents.last as TemplateVariable

		val firstMacroTestStepContext = macroTestStepContext(macroCollection) => [
			step = testStep("myothercall").withReferenceToVariable(variable)
		]
		val secondMacroTestStepContext = macroCollection.macros.last.contexts.head as MacroTestStepContext

		classUnderTest.macroUseStack = #[secondMacroTestStepContext, firstMacroTestStepContext]
		
		return result
	}
}
