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
import org.testeditor.tcl.EnvParam
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

import static org.testeditor.tml.TmlPackage.Literals.*
import org.testeditor.tsl.StepContentValue

class TclJvmModelInferrer extends AbstractModelInferrer {

	@Inject extension JvmTypesBuilder
	@Inject extension ModelUtil
	@Inject extension TclModelUtil
	@Inject TclAssertCallBuilder assertCallBuilder
	@Inject IQualifiedNameProvider nameProvider

	def dispatch void infer(TclModel model, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		model.test?.infer(acceptor, isPreIndexingPhase)
	}

	private def String envParamToVarName(String name) {
		return "env_" + name
	}

	def dispatch void infer(TestCase test, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		val envParams = test.model.envParams
		acceptor.accept(test.toClass(nameProvider.getFullyQualifiedName(test))) [
			documentation = '''Generated from «test.eResource.URI»'''
			// Create variables for used fixture types
			for (fixtureType : test.fixtureTypes) {
				members += toField(test, fixtureType.fixtureFieldName, typeRef(fixtureType)) [
					initializer = '''new «fixtureType»()'''
				]
			}
			for (envParam : envParams) {
				members += toField(envParam, envParam.name.envParamToVarName, typeRef(String)) [
					initializer = '''System.getenv("«envParam.name»")'''
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

	def void generateMethodBody(TestCase test, ITreeAppendable output, Set<EnvParam> envParams) {
		envParams.forEach[generateEnvCheck(output)]
		test.steps.forEach[generate(output.trace(it), envParams)]
	}

	private def void generateEnvCheck(EnvParam envParam, ITreeAppendable output) {
		output.append('''
			if (!System.getenv().containsKey("«envParam.name»")) {
				org.junit.Assert.fail("Required system property '«envParam.name»' is missing in environment.");
			}
		''').newLine
	}

	private def void generate(SpecificationStepImplementation step, ITreeAppendable output, Set<EnvParam> envParams) {
		val comment = '''/* «step.contents.restoreString» */'''
		output.newLine
		output.append(comment).newLine
		step.contexts.forEach[generateContext(output.trace(it), #[], envParams)]
	}

	private def dispatch void generateContext(MacroTestStepContext context, ITreeAppendable output,
		Iterable<MacroTestStepContext> macroUseStack, Set<EnvParam> envParams) {
		output.newLine
		val macro = context.findMacroDefinition
		output.append('''// Macro start: «context.macroModel.name» - «macro.template.normalize»''').newLine
		macro.contexts.forEach[generateContext(output.trace(it), #[context] + macroUseStack, envParams)]
		output.newLine
		output.append('''// Macro end: «context.macroModel.name» - «macro.template.normalize»''').newLine
	}

	private def dispatch void generateContext(ComponentTestStepContext context, ITreeAppendable output,
		Iterable<MacroTestStepContext> macroUseStack, Set<EnvParam> envParams) {
		output.newLine
		output.append('''// Component: «context.component.name»''').newLine
		context.steps.forEach[generate(output.trace(it), macroUseStack, envParams)]
	}

	protected def void generate(TestStep step, ITreeAppendable output, Iterable<MacroTestStepContext> macroUseStack,
		Set<EnvParam> envParams) {
		output.newLine
		output.append('''// - «step.contents.restoreString»''').newLine
		toUnitTestCodeLine(step, output, macroUseStack, envParams)
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
		Iterable<MacroTestStepContext> macroUseStack, Set<EnvParam> envParams) {
		output.append(assertCallBuilder.build(step.expression)).newLine
	}

	private def dispatch void toUnitTestCodeLine(TestStep step, ITreeAppendable output,
		Iterable<MacroTestStepContext> macroUseStack, Set<EnvParam> envParams) {
		val interaction = step.interaction
		if (interaction !== null) {
			val fixtureField = interaction.defaultMethod?.typeReference?.type?.fixtureFieldName
			val operation = interaction.defaultMethod?.operation
			if (fixtureField !== null && operation !== null) {
				step.maybeCreateAssignment(operation, output)
				output.trace(interaction.defaultMethod) => [
					val codeLine = '''«fixtureField».«operation.simpleName»(«getParameterList(step, interaction, macroUseStack, envParams)»);'''
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
		Iterable<MacroTestStepContext> macroUseStack, Set<EnvParam> envParams) {
		val mapping = getVariableToValueMapping(step, interaction.template)
		val stepContents = interaction.defaultMethod.parameters.map [ templateVariable |
			val stepContent = mapping.get(templateVariable)
			val stepContentDereferenced = if (stepContent instanceof StepContentVariableReference) {
					stepContent.dereferenceVariableReference(macroUseStack, envParams)
				} else {
					stepContent
				}
			return stepContentDereferenced
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
	private def dispatch Iterable<String> generateCallParameters(StepContentValue stepContent, JvmTypeReference expectedType,
		InteractionType interaction) {
		if (expectedType.qualifiedName == String.name) {
			return #['''"«stepContent.value»"''']
		} else {
			return #[stepContent.value]
		}
	}

	/**
	 * generate the parameter-code passed to the fixture call depending on the type of the step content
	 */
	private def dispatch Iterable<String> generateCallParameters(StepContentVariableReference stepContent,
		JvmTypeReference expectedType, InteractionType interaction) {
		if (expectedType.qualifiedName.equals(String.name)) {
			return #[stepContent.variable.name.envParamToVarName]
		} else {
			throw new RuntimeException('''Environment variable '«stepContent.variable?.name»' (always of type String) is used where type '«expectedType.qualifiedName»' is expected.''')
		}
	}

	/**
	 * resolve dereferenced variable (in macro) with call site value (recursively if necessary)
	 */
	private def StepContent dereferenceVariableReference(StepContentVariableReference referencedVariable,
		Iterable<MacroTestStepContext> macroUseStack, Set<EnvParam> envParams) {

		if (macroUseStack.empty && envParams.map[name].exists[equals(referencedVariable.variable.name)]) {
			return referencedVariable
		}

		val callSiteMacroContext = macroUseStack.head
		val macroCalled = callSiteMacroContext.findMacroDefinition

		val varValMap = getVariableToValueMapping(callSiteMacroContext.step, macroCalled.template)
		val varKey = varValMap.keySet.findFirst[name.equals(referencedVariable.variable.name)]
		val callSiteParameter = varValMap.get(varKey)

		if (callSiteParameter instanceof StepContentVariableReference) {
			return callSiteParameter.dereferenceVariableReference(macroUseStack.tail, envParams)
		} else {
			return callSiteParameter
		}
	}

}
