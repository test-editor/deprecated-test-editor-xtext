/*******************************************************************************
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 *******************************************************************************/
package org.testeditor.tcl.dsl.jvmmodel

import com.google.inject.Inject
import java.util.Set
import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.common.types.JvmType
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable
import org.eclipse.xtext.xbase.jvmmodel.AbstractModelInferrer
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder
import org.testeditor.aml.InteractionType
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.VariableReference
import org.testeditor.tcl.EnvironmentVariableReference
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tml.AssertionTestStep
import org.testeditor.tml.ComponentTestStepContext
import org.testeditor.tml.MacroTestStepContext
import org.testeditor.tml.StepContentElement
import org.testeditor.tml.StepContentVariableReference
import org.testeditor.tml.TestStep
import org.testeditor.tml.TestStepWithAssignment
import org.testeditor.tsl.StepContent
import org.testeditor.tsl.StepContentValue

import static org.testeditor.tml.TmlPackage.Literals.*

class TclJvmModelInferrer extends AbstractModelInferrer {

	@Inject extension JvmTypesBuilder
	@Inject extension ModelUtil
	@Inject extension TclModelUtil
	@Inject TclAssertCallBuilder assertCallBuilder
	@Inject IQualifiedNameProvider nameProvider

	def dispatch void infer(TclModel model, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		model.test?.infer(acceptor, isPreIndexingPhase)
	}

	private def String variableReferenceToVarName(VariableReference varRef) {
		return "env_" + varRef.name
	}

	def dispatch void infer(TestCase test, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		val envParams = test.model.environmentVariableReferences
		acceptor.accept(test.toClass(nameProvider.getFullyQualifiedName(test))) [
			documentation = '''Generated from «test.eResource.URI»'''
			// Create variables for used fixture types
			members += test.fixtureTypes.map [ fixtureType |
				toField(test, fixtureType.fixtureFieldName, fixtureType.typeRef) [
					initializer = '''new «fixtureType»()'''
				]
			]
			// create variables for required environment variables
			members += envParams.map [ environmentVariableReference |
				environmentVariableReference.toField(environmentVariableReference.variableReferenceToVarName,
					typeRef(String)) [
					initializer = '''System.getenv("«environmentVariableReference.name»")'''
				]
			]
			if (! envParams.empty) {
				members += test.toMethod('checkEnvironmentVariablesOnExistence', typeRef(Void.TYPE)) [
					exceptions += typeRef(Exception)
					annotations += annotationRef('org.junit.Before') // make sure that junit is in the classpath of the workspace containing the dsl
					body = [
						val output = trace(test, true)
						envParams.forEach[generateEnvironmentVariableAssertion(output)]
					]
				]
			}
			// Create test method
			members += test.toMethod('execute', typeRef(Void.TYPE)) [
				exceptions += typeRef(Exception)
				annotations += annotationRef('org.junit.Test') // make sure that junit is in the classpath of the workspace containing the dsl
				body = [test.generateMethodBody(trace(test, true), envParams)]
			]
		]

	}

	def void generateMethodBody(TestCase test, ITreeAppendable output,
		Iterable<EnvironmentVariableReference> EnvironmentVariableReferences) {
		test.steps.forEach[generate(output.trace(it), EnvironmentVariableReferences)]
	}

	private def void generateEnvironmentVariableAssertion(EnvironmentVariableReference EnvironmentVariableReference,
		ITreeAppendable output) {
		output.append('''org.junit.Assert.assertNotNull(«EnvironmentVariableReference.variableReferenceToVarName»);''').
			newLine
	}

	private def void generate(SpecificationStepImplementation step, ITreeAppendable output,
		Iterable<EnvironmentVariableReference> EnvironmentVariableReferences) {
		val comment = '''/* «step.contents.restoreString» */'''
		output.newLine
		output.append(comment).newLine
		step.contexts.forEach[generateContext(output.trace(it), #[], EnvironmentVariableReferences)]
	}

	private def dispatch void generateContext(MacroTestStepContext context, ITreeAppendable output,
		Iterable<MacroTestStepContext> macroUseStack,
		Iterable<EnvironmentVariableReference> EnvironmentVariableReferences) {
		output.newLine
		val macro = context.findMacroDefinition
		output.append('''// Macro start: «context.macroModel.name» - «macro.template.normalize»''').newLine
		macro.contexts.forEach [
			generateContext(output.trace(it), #[context] + macroUseStack, EnvironmentVariableReferences)
		]
		output.newLine
		output.append('''// Macro end: «context.macroModel.name» - «macro.template.normalize»''').newLine
	}

	private def dispatch void generateContext(ComponentTestStepContext context, ITreeAppendable output,
		Iterable<MacroTestStepContext> macroUseStack,
		Iterable<EnvironmentVariableReference> EnvironmentVariableReferences) {
		output.newLine
		output.append('''// Component: «context.component.name»''').newLine
		context.steps.forEach[generate(output.trace(it), macroUseStack, EnvironmentVariableReferences)]
	}

	protected def void generate(TestStep step, ITreeAppendable output,
		Iterable<MacroTestStepContext> macroUseStack,
		Iterable<EnvironmentVariableReference> EnvironmentVariableReferences) {
		output.newLine
		output.append('''// - «step.contents.restoreString»''').newLine
		toUnitTestCodeLine(step, output, macroUseStack, EnvironmentVariableReferences)
	}

	/**
	 * @return all {@link JvmType} of all fixtures that are referenced.
	 */
	private def Set<JvmType> getFixtureTypes(TestCase test) {
		val allTestStepContexts = test.steps.map[contexts].flatten.filterNull
		return allTestStepContexts.map[testStepFixtureTypes].flatten.toSet
	}

	private def dispatch Set<JvmType> getTestStepFixtureTypes(ComponentTestStepContext context) {
		val interactionTypes = getAllInteractionTypes(context.component).toSet
		val fixtureTypes = interactionTypes.map[fixtureType].filterNull.toSet
		return fixtureTypes
	}

	private def dispatch Set<JvmType> getTestStepFixtureTypes(MacroTestStepContext context) {
		val macro = context.findMacroDefinition
		if (macro !== null) {
			return macro.contexts.filterNull.map[testStepFixtureTypes].flatten.toSet
		} else {
			return #{}
		}
	}

	private def String getFixtureFieldName(JvmType fixtureType) {
		return fixtureType.simpleName.toFirstLower
	}

	private def dispatch void toUnitTestCodeLine(AssertionTestStep step, ITreeAppendable output,
		Iterable<MacroTestStepContext> macroUseStack,
		Iterable<EnvironmentVariableReference> EnvironmentVariableReferences) {
		output.append(assertCallBuilder.build(step.expression)).newLine
	}

	private def dispatch void toUnitTestCodeLine(TestStep step, ITreeAppendable output,
		Iterable<MacroTestStepContext> macroUseStack,
		Iterable<EnvironmentVariableReference> EnvironmentVariableReferences) {
		val interaction = step.interaction
		if (interaction !== null) {
			val fixtureField = interaction.defaultMethod?.typeReference?.type?.fixtureFieldName
			val operation = interaction.defaultMethod?.operation
			if (fixtureField !== null && operation !== null) {
				step.maybeCreateAssignment(operation, output)
				output.trace(interaction.defaultMethod) => [
					val codeLine = '''«fixtureField».«operation.simpleName»(«getParameterList(step, interaction, macroUseStack, EnvironmentVariableReferences)»);'''
					append(codeLine) // please call with string, since tests checks against expected string which fails for passing ''' directly
				]
			} else {
				output.append('''// TODO interaction type '«interaction.name»' does not have a proper method reference''')
			}
		} else if (step.componentContext != null) {
			output.append('''// TODO could not resolve '«step.componentContext.component.name»' - «step.contents.restoreString»''')
		} else {
			output.append('''// TODO could not resolve unknown component - «step.contents.restoreString»''')
		}
	}

	def void maybeCreateAssignment(TestStep step, JvmOperation operation, ITreeAppendable output) {
		if (step instanceof TestStepWithAssignment) {
			output.trace(step, TEST_STEP_WITH_ASSIGNMENT__VARIABLE_NAME, 0) => [
				// TODO should we use output.declareVariable here?
				// val variableName = output.declareVariable(step.variableName, step.variableName)
				output.append('''«operation.returnType.identifier» «step.variableName» = ''')
			]
		}
	}

	// TODO we could also trace the parameters here
	private def String getParameterList(TestStep step, InteractionType interaction,
		Iterable<MacroTestStepContext> macroUseStack,
		Iterable<EnvironmentVariableReference> EnvironmentVariableReferences) {
		val mapping = getVariableToValueMapping(step, interaction.template)
		val stepContents = interaction.defaultMethod.parameters.map [ templateVariable |
			val stepContent = mapping.get(templateVariable)
			val stepContentResolved = if (stepContent instanceof StepContentVariableReference) {
					stepContent.resolveVariableReference(macroUseStack,
						EnvironmentVariableReferences)
				} else {
					stepContent
				}
			return stepContentResolved
		]
		val typedValues = newArrayList
		stepContents.forEach [ stepContent, i |
			val jvmParameter = interaction.getTypeOfFixtureParameter(i)
			typedValues += stepContent.generateCallParameters(jvmParameter, interaction)
		]
		return typedValues.join(', ')
	}

	/**
	 * generate the parameter-code passed to the fixture call depending on the type of the step content
	 */
	private def dispatch Iterable<String> generateCallParameters(StepContentElement stepContent,
		JvmTypeReference expectedType, InteractionType interaction) {
		val element = stepContent.componentElement
		val locator = '''"«element.locator»"'''
		if (interaction.defaultMethod.locatorStrategyParameters.size>0) {
			// use element locator strategy if present, else use default of interaction
			val locatorStrategy = element.locatorStrategy ?: interaction.locatorStrategy
			return #[locator, locatorStrategy.qualifiedName] // locatorStrategy is the parameter right after locator (convention)
		} else {
			return #[locator]
		}
	}

	/**
	 * generate the parameter-code passed to the fixture call depending on the type of the step content
	 */
	private def dispatch Iterable<String> generateCallParameters(StepContentValue stepContentValue, JvmTypeReference expectedType,
		InteractionType interaction) {
		if (expectedType.qualifiedName == String.name) {
			return #['''"«stepContentValue.value»"''']
		} else {
			return #[stepContentValue.value]
		}
	}

	/**
	 * generate the parameter-code passed to the fixture call depending on the type of the step content
	 */
	private def dispatch Iterable<String> generateCallParameters(StepContentVariableReference stepContent,
		JvmTypeReference expectedType, InteractionType interaction) {
		if (expectedType.qualifiedName.equals(String.name)) {
			return #[stepContent.variable.variableReferenceToVarName]
		} else {
			throw new RuntimeException('''Environment variable '«stepContent.variable.name»' (always of type String) is used where type '«expectedType.qualifiedName»' is expected.''')
		}
	}

	/**
	 * resolve dereferenced variable (in macro) with call site value (recursively if necessary).
	 *
	 * <pre>
	 * given the following scenario (this is just one example):
	 *   Tcl uses Macro A -> which again uses a Macro B -> which uses a component interaction
	 *   => referencedVariable is the variable name in the context of B
	 *    macroUseStack = #[ B, A ]   (call usage in reverse order)
	 *    environmentVariableReferences = required environment vars of tcl (if present)
	 *
	 * wanted:
	 *   in order to get the parameter/value that should actually be passed to the
	 *   transitively called fixture method, the value/environment variable of the
	 *   original call site within the tcl must be found.
	 *
	 *   as long as the the macroUseStack is not empty and the parameter used for the call
	 *   is again a variable reference, this method recursively calls itself:
	 *     the referencedVariable is decoded to the parameter name as it is used in the
	 *     enclosing macro call context and the top is poped off the stack
	 *  </pre>
	 *
	 * @see org.testeditor.tcl.dsl.validation.TclParameterUsageValidatorTest
	 *
	 */
	private def StepContent resolveVariableReference(
		StepContentVariableReference referencedVariable,
		Iterable<MacroTestStepContext> macroUseStack,
		Iterable<EnvironmentVariableReference> environmentVariableReferences) {

		if (macroUseStack.empty && environmentVariableReferences.map[name].exists [
			equals(referencedVariable.variable.name)
		]) {
			return referencedVariable
		}

		val callSiteMacroContext = macroUseStack.head
		val macroCalled = callSiteMacroContext.findMacroDefinition

		val varValMap = getVariableToValueMapping(callSiteMacroContext.step,
			macroCalled.template)
		val varKey = varValMap.keySet.findFirst [
			name.equals(referencedVariable.variable.name)
		]
		val callSiteParameter = varValMap.get(varKey)

		if (callSiteParameter instanceof StepContentVariableReference) {
			return callSiteParameter.resolveVariableReference(macroUseStack.tail,
				environmentVariableReferences)
		} else {
			return callSiteParameter
		}
	}

}
