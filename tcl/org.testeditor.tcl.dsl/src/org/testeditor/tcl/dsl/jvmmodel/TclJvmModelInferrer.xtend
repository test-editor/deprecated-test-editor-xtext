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
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable
import org.eclipse.xtext.xbase.jvmmodel.AbstractModelInferrer
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder
import org.testeditor.aml.InteractionType
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.Template
import org.testeditor.aml.TemplateText
import org.testeditor.aml.TemplateVariable
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tml.AssertionTestStep
import org.testeditor.tml.ComponentTestStepContext
import org.testeditor.tml.Macro
import org.testeditor.tml.MacroTestStepContext
import org.testeditor.tml.StepContentDereferencedVariable
import org.testeditor.tml.StepContentElement
import org.testeditor.tml.TestStep
import org.testeditor.tml.TestStepWithAssignment

import static org.testeditor.tml.TmlPackage.Literals.*

class TclJvmModelInferrer extends AbstractModelInferrer {

	@Inject extension JvmTypesBuilder
	@Inject extension TclModelUtil
	@Inject TclAssertCallBuilder assertCallBuilder
	@Inject IQualifiedNameProvider nameProvider
	@Inject ModelUtil amlModelUtil

	def dispatch void infer(TclModel model, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		model.test?.infer(acceptor, isPreIndexingPhase)
	}

	def dispatch void infer(TestCase test, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		acceptor.accept(test.toClass(nameProvider.getFullyQualifiedName(test))) [
			documentation = '''Generated from «test.eResource.URI»'''
			// Create variables for used fixture types
			for (fixtureType : test.fixtureTypes) {
				members += toField(test, fixtureType.fixtureFieldName, typeRef(fixtureType)) [
					initializer = '''new «fixtureType»()'''
				]
			}
			// Create test method
			members += test.toMethod('execute', typeRef(Void.TYPE)) [
				exceptions += typeRef(Exception)
				annotations += annotationRef('org.junit.Test') // make sure that junit is in the classpath of the workspace containing the dsl
				body = [test.generateMethodBody(trace(test, true))]
			]
		]

	}

	private def void generateMethodBody(TestCase test, ITreeAppendable output) {
		test.steps.forEach[generate(output.trace(it))]
	}

	private def void generate(SpecificationStepImplementation step, ITreeAppendable output) {
		val comment = '''/* «step.contents.restoreString» */'''
		output.newLine
		output.append(comment).newLine
		step.contexts.forEach[generateContext(output.trace(it), #[])]
	}

	private def dispatch void generateContext(MacroTestStepContext context, ITreeAppendable output,
		Iterable<MacroTestStepContext> scope) {
		output.newLine
		val macro = context.findMacroDefinition
		output.append('''// Macro start: «context.macroModel.name» - «macro.template.normalize»''').newLine
		macro.contexts.forEach[generateContext(output.trace(it), #[context] + scope)]
		output.newLine
		output.append('''// Macro end: «context.macroModel.name» - «macro.template.normalize»''').newLine
	}

	private def dispatch void generateContext(ComponentTestStepContext context, ITreeAppendable output,
		Iterable<MacroTestStepContext> scope) {
		output.newLine
		output.append('''// Component: «context.component.name»''').newLine
		context.steps.forEach[generate(output.trace(it), scope)]
	}

	private def void generate(TestStep step, ITreeAppendable output, Iterable<MacroTestStepContext> scope) {
		output.newLine
		output.append('''// - «step.contents.restoreString»''').newLine
		toUnitTestCodeLine(step, output, scope)
	}

	/**
	 * @return all {@link JvmType} of all fixtures that are referenced.
	 */
	private def Set<JvmType> getFixtureTypes(TestCase test) {
		val allTestStepContexts = test.steps.map[contexts].flatten.filterNull
		return allTestStepContexts.map[testStepFixtureTypes].flatten.toSet
	}

	private def dispatch Set<JvmType> getTestStepFixtureTypes(ComponentTestStepContext context) {
		val interactionTypes = amlModelUtil.getAllInteractionTypes(context.component).toSet
		val fixtureTypes = interactionTypes.map[amlModelUtil.getFixtureType(it)].filterNull.toSet
		return fixtureTypes
	}

	private def dispatch Set<JvmType> getTestStepFixtureTypes(MacroTestStepContext context) {
		context.findMacroDefinition?.contexts.filterNull.map[testStepFixtureTypes].flatten.toSet
	}

	private def String getFixtureFieldName(JvmType fixtureType) {
		return fixtureType.simpleName.toFirstLower
	}

	private def dispatch void toUnitTestCodeLine(AssertionTestStep step, ITreeAppendable output,
		Iterable<MacroTestStepContext> scope) {
		output.append(assertCallBuilder.build(step.expression)).newLine
	}

	private def dispatch void toUnitTestCodeLine(TestStep step, ITreeAppendable output,
		Iterable<MacroTestStepContext> scope) {
		val interaction = step.interaction
		if (interaction !== null) {
			val fixtureField = interaction.defaultMethod?.typeReference?.type?.fixtureFieldName
			val operation = interaction.defaultMethod?.operation
			if (fixtureField !== null && operation !== null) {
				step.maybeCreateAssignment(operation, output)
				output.trace(interaction.defaultMethod) => [
					append('''«fixtureField».«operation.simpleName»(«getParameterList(step, interaction,scope)»);''')
				]
			} else {
				output.
					append('''// TODO interaction type '«interaction.name»' does not have a proper method reference''')
			}
		} else if (step.componentContext != null) {
			output.
				append('''// TODO could not resolve '«step.componentContext.component.name»' - «step.contents.restoreString»''')
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
		Iterable<MacroTestStepContext> scope) {
		val mapping = getVariableToValueMapping(step, interaction)
		val values = interaction.defaultMethod.parameters.map [ templateVariable |
			val stepContent = mapping.get(templateVariable)
			if (stepContent instanceof StepContentElement) {
				val element = stepContent.componentElement
				return element.locator
			} else if (stepContent instanceof StepContentDereferencedVariable) {
				return stepContent.dereferenceMacroVariableReference(scope)
			} else {
				return stepContent.value
			}
		]
		val typedValues = newArrayList
		val operationParameters = interaction.defaultMethod.operation.parameters
		values.forEach [ value, i |
			val jvmParameter = operationParameters.get(i)
			if (jvmParameter.parameterType.qualifiedName == String.name) {
				typedValues += '''"«value»"'''
			} else {
				typedValues += value
			}
		]
		return typedValues.join(', ')
	}

	/**
	 * Given a template find the index of a content element in the call location
	 */
	private def int templateIndexToCallIndex(Template template, int index) {
		val idxCorrected = template.contents.map [
			if (it instanceof TemplateText) {
				return (it as TemplateText).value.split(" ").size // the text of a template may end up as several tokens @ callsite
			} else {
				return 1 // no change
			}
		].take(index).reduce[p1, p2|p1 + p2]
		return idxCorrected
	}

	/**
	 * get the index of the given variable in the template definition
	 */
	private def Integer indexOfVariable(Template template, String variable) {
		val index = template.contents.indexed.filter [
			if (value instanceof TemplateVariable) {
				val comparator = '''«variable»'''
				val result = comparator == (value as TemplateVariable).name
				return result
			} else {
				return false
			}
		].head?.key
		return index
	}

	/**
	 * resolve dereferenced variable in macro with call site value (recursively if necessary)
	 */
	private def String dereferenceMacroVariableReference(StepContentDereferencedVariable dereferencedVariable,
		Iterable<MacroTestStepContext> scope) {
		val callSiteMacroContext = scope.head
		val macroCalled = callSiteMacroContext.findMacroDefinition
		val index = macroCalled.template.indexOfVariable(dereferencedVariable.value)
		val idxCorrected = macroCalled.template.templateIndexToCallIndex(index)
		val callSiteParameter = callSiteMacroContext.step.contents.get(idxCorrected)
		if (callSiteParameter instanceof StepContentDereferencedVariable) {
			return callSiteParameter.dereferenceMacroVariableReference(scope.tail)
		} else {
			return callSiteParameter.value
		}
	}

}
