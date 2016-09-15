package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.junit.Test
import org.testeditor.aml.TemplateVariable
import org.testeditor.aml.Variable
import org.testeditor.aml.dsl.tests.AmlModelGenerator
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.dsl.tests.TclModelGenerator
import org.testeditor.tsl.StepContent
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTestWithDummyComponent

class MacroCallVariableResolverTest extends AbstractParserTestWithDummyComponent {
	@Inject extension TclModelGenerator
	@Inject extension AmlModelGenerator

	@Inject MacroCallVariableResolver classUnderTest

	@Test
	def void testResolvingReferenceToAssignmentVariable() {
		// given
		classUnderTest.macroUseStack = #[] // empty call stack
		val assignmentVariableRef = variableReference => [variable = assignmentVariable("some")]

		// when
		val resolvedVariable = classUnderTest.resolveVariableReference(assignmentVariableRef)

		// then
		resolvedVariable.assertReferenceToVariable(assignmentVariableRef.variable)
	}
	
	@Test
	def void testResolvingReferenceToEnvironmentVariable() {
		// given
		classUnderTest.macroUseStack = #[] // empty call stack
		val environmentVariableRef = variableReference => [variable = environmentVariables("some").head]

		// when
		val resolvedVariable = classUnderTest.resolveVariableReference(environmentVariableRef)

		// then
		resolvedVariable.assertReferenceToVariable(environmentVariableRef.variable)
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
		resolvedVariable.assertReferenceToVariable(environmentVariable)
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
		resolvedVariable.assertReferenceToVariable(environmentVariable)
	}
	
	@Test
	def void testResolvingReferenceToAssignmentVariableThroughSingleMacroCall() {
		// given
		val assignmentVariable = assignmentVariable("some")
		val macroParameter = parameterForCallingSingleMacro(assignmentVariable)
		val macroParameterReference = variableReference => [variable = macroParameter] 

		// when
		val resolvedVariable = classUnderTest.resolveVariableReference(macroParameterReference)

		// then
		resolvedVariable.assertReferenceToVariable(assignmentVariable)
	}

	@Test
	def void testResolvingReferenceToAssignmentVariableThroughChainedMacroCall() {
		// given
		val assignmentVariable = assignmentVariable("some")
		val macroParameter = macroParameterAtEndOfCallChain(assignmentVariable)
		val macroParameterReference = variableReference => [variable = macroParameter] 
		
		// when
		val resolvedVariable = classUnderTest.resolveVariableReference(macroParameterReference)

		// then
		resolvedVariable.assertReferenceToVariable(assignmentVariable)
	}

	/** assert that the resolvedVariable is a reference to the given variable
	 */	
	private def void assertReferenceToVariable(StepContent resolvedVariable, Variable variable) {
		resolvedVariable.assertInstanceOf(VariableReference).variable.assertSame(variable)
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
			steps += testStep("mycall").withReferenceToVariable(variable)
		]

		classUnderTest.macroUseStack = #[macroTestStepContext.steps.head as TestStep]
		
		return result

	}
	
	/** build a context where a regular test steps calls a macro which in turn calls another macro 
	 *  => result is the macro parameter name of the last macro called
	 * 
	 *  the (macro) call stack is passed to the class under test
	 */
	private def TemplateVariable macroParameterAtEndOfCallChain(Variable variable) {
		val macroCollection = macroCollection("MacroCollection") // must be assigned before usage in macro creation (see below)
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
					steps += testStep("mycall").withReferenceToVariable(parameter)
				]
			]
		]

		val result = macroCollection.macros.head.template.contents.last as TemplateVariable

		val firstMacroTestStepContext = macroTestStepContext(macroCollection) => [
			steps += testStep("myothercall").withReferenceToVariable(variable)
		]
		val secondMacroTestStepContext = macroCollection.macros.last.contexts.head as MacroTestStepContext

		classUnderTest.macroUseStack = #[secondMacroTestStepContext.steps.head as TestStep, firstMacroTestStepContext.steps.head as TestStep]
		
		return result
	}
}
