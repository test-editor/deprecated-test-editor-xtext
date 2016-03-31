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
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.TestStepWithAssignment
import org.testeditor.tcl.util.TclModelUtil

import static org.testeditor.tcl.TclPackage.Literals.*

class TclJvmModelInferrer extends AbstractModelInferrer {

	@Inject extension JvmTypesBuilder
	@Inject extension TclModelUtil
	@Inject IQualifiedNameProvider nameProvider
	
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
				body = [test.generateMethodBody(trace(test))]
			]
		]

	}

	private def void generateMethodBody(TestCase test, ITreeAppendable output) {
		test.steps.forEach[generate(output.trace(it))]
	}

	private def void generate(SpecificationStepImplementation step, ITreeAppendable output) {
		val comment = '''/* «step.contents.restoreString» */'''
		output.newLine
		output.append(comment)
		step.contexts.forEach[generate(output.trace(it))]
	}

	private def void generate(TestStepContext context, ITreeAppendable output) {
		output.newLine
		output.append('''// Component: «context.component.name»''')
		context.steps.forEach[generate(output.trace(it))]
	}
	
	private def void generate(TestStep step, ITreeAppendable output) {
		output.newLine
		output.append('''// - «step.contents.restoreString»''')
		generateFeatureCall(step, output)
	}

	/**
	 * @return all {@link JvmType} of all fixtures that are referenced.
	 */
	private def Set<JvmType> getFixtureTypes(TestCase test) {
		val components = test.steps.map[contexts].flatten.filterNull.map[component].filterNull
		// TODO the part from here should go somewhere in Aml ModelUtil
		val interactionTypes = components.map[type?.interactionTypes].flatten.filterNull.toSet
		val fixtureTypes = interactionTypes.map[defaultMethod?.typeReference?.type].filterNull.toSet
		return fixtureTypes
	}

	private def String getFixtureFieldName(JvmType fixtureType) {
		return fixtureType.simpleName.toFirstLower
	}

	private def void generateFeatureCall(TestStep step, ITreeAppendable output) {
		output.newLine
		val context = step.context
		val interaction = step.interaction
		if (interaction !== null) {
			val fixtureField = interaction.defaultMethod?.typeReference?.type?.fixtureFieldName
			val operation = interaction.defaultMethod?.operation
			if (fixtureField !== null && operation !== null) {
				step.maybeCreateAssignment(operation, output)
				output.trace(interaction.defaultMethod) => [
					append('''«fixtureField».«operation.simpleName»(«getParameterList(step, interaction)»);''')
				]
			} else {
				output.append('''// TODO interaction type '«interaction.name»' does not have a proper method reference''')
			}
		} else {
			output.append('''// TODO could not resolve '«context.component.name»' - «step.contents.restoreString»''')
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
	private def String getParameterList(TestStep step, InteractionType interaction) {
		val mapping = getVariableToValueMapping(step, interaction)
		val values = interaction.defaultMethod.parameters.map [ templateVariable |
			val stepContent = mapping.get(templateVariable)
			if (stepContent instanceof StepContentElement) {
				val element = stepContent.componentElement
				return element.locator
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

}

