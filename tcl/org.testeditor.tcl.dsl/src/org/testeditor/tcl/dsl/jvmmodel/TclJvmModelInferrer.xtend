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
import org.apache.commons.lang3.StringEscapeUtils
import org.eclipse.xtext.common.types.JvmConstructor
import org.eclipse.xtext.common.types.JvmDeclaredType
import org.eclipse.xtext.common.types.JvmField
import org.eclipse.xtext.common.types.JvmGenericType
import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.common.types.JvmType
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.common.types.JvmVisibility
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable
import org.eclipse.xtext.xbase.jvmmodel.AbstractModelInferrer
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder
import org.slf4j.LoggerFactory
import org.testeditor.aml.InteractionType
import org.testeditor.aml.ModelUtil
import org.testeditor.fixture.core.AbstractTestCase
import org.testeditor.fixture.core.TestRunReportable
import org.testeditor.fixture.core.TestRunReporter.SemanticUnit
import org.testeditor.tcl.AbstractTestStep
import org.testeditor.tcl.AssertionTestStep
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.EnvironmentVariable
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.SetupAndCleanupProvider
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.TestConfiguration
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.TestStepWithAssignment
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.VariableReferenceMapAccess
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tsl.StepContentValue

import static org.testeditor.tcl.TclPackage.Literals.*
import org.testeditor.fixture.core.TestRunReporter

class TclJvmModelInferrer extends AbstractModelInferrer {

	static val logger = LoggerFactory.getLogger(TclJvmModelInferrer)

	@Inject extension JvmTypesBuilder
	@Inject extension ModelUtil
	@Inject extension TclModelUtil
	@Inject TclAssertCallBuilder assertCallBuilder
	@Inject IQualifiedNameProvider nameProvider
	@Inject JvmModelHelper jvmModelHelper
	@Inject TclExpressionBuilder expressionBuilder
	@Inject MacroCallVariableResolver macroCallVariableResolver
	@Inject TestRunReporterGenerator testRunReporterGenerator 
		
	def dispatch void infer(TclModel model, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		model.test?.infer(acceptor, isPreIndexingPhase)
		model.config?.infer(acceptor, isPreIndexingPhase)
	}

	/**
	 * Performs the minimal initialization to a class - its super type and
	 * its variables.
	 */
	private def JvmGenericType toClass(SetupAndCleanupProvider element, boolean isPreIndexingPhase) {
		if (isPreIndexingPhase) {
			return element.toClass(nameProvider.getFullyQualifiedName(element))
		} else {
			return element.toClass(nameProvider.getFullyQualifiedName(element)) [
				// Add super type to the element
				addSuperType(element)

				// Create variables for used fixture types
				members += createFixtureVariables(element)
			]
		}
	}

	/**
	 * First perform common operations like variable initialization and then dispatch to subclass specific operations.
	 */
	def dispatch void infer(SetupAndCleanupProvider element, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		// Create the class with eager initialization
		val generatedClass = element.toClass(isPreIndexingPhase)

		// Stuff that can be done in late initialization
		acceptor.accept(generatedClass) [
			documentation = '''Generated from «element.eResource.URI»'''

			// Create constructor, if initialization of instantiated types with reporter is necessary
			val typesToInitWithReporter = getAllInstantiatedTypesImplementingTestRunReportable(element, generatedClass)
			if (!typesToInitWithReporter.empty) {
				members += element.createConstructor(typesToInitWithReporter)
			}
			// Create @Before method if relevant
			if (element.setup !== null) {
				members += element.createSetupMethod
			}
			// Create @After method if relevant
			if (element.cleanup !== null) {
				members += element.createCleanupMethod
			}

			// subclass specific operations
			infer(element)
		]
	}

	private def Iterable<JvmType> getAllTypesToInstantiate(SetupAndCleanupProvider element, JvmDeclaredType generatedClass) {
		val fixtureTypes = element.fixtureTypes
		val accessibleSuperFieldTypes = jvmModelHelper.getAllAccessibleSuperTypeFields(generatedClass).map [
			it.type.type
		]
		return fixtureTypes.filter[!accessibleSuperFieldTypes.contains(it)]
	}
	
	private def Iterable<JvmDeclaredType> getAllInstantiatedTypesImplementingTestRunReportable(SetupAndCleanupProvider element, JvmDeclaredType generatedClass) {
		return getAllTypesToInstantiate(element, generatedClass) //
			.filter(JvmDeclaredType) //
			.filter [extendedInterfaces.map[qualifiedName].exists[equals(TestRunReportable.canonicalName)]]
	}
	
	def JvmConstructor createConstructor(SetupAndCleanupProvider element,
			Iterable<JvmDeclaredType> typesToInitWithReporter) {
		return toConstructor(element) [
			body = [
				typesToInitWithReporter.forEach [ fixtureType |
					append('((').
					append(typeRef(TestRunReportable).type).
					append(''')«fixtureType.fixtureFieldName»).initWithReporter(«reporterFieldName»);''')
				]
			]
		]
	}

	private def void addSuperType(JvmGenericType result, SetupAndCleanupProvider element) {
		if (element instanceof TestConfiguration) {
			result.superTypes += typeRef(AbstractTestCase)
		}		
		if (element instanceof TestCase) {
			// Inherit from configuration, if set - need to be done before 
			if (element.config !== null) {
				result.superTypes += typeRef(element.config.toClass(false))
			} else {
				result.superTypes += typeRef(AbstractTestCase)
			}
		} // TODO allow explicit definition of super type in TestConfiguration
	}

	private def dispatch void infer(JvmGenericType result, TestConfiguration element) {
		result.abstract = true
	}

	private def dispatch void infer(JvmGenericType result, TestCase element) {
		// create variables for required environment variables
		val envParams = element.model.environmentVariables
		result.members += envParams.map [ environmentVariable |
			environmentVariable.toField(expressionBuilder.variableToVarName(environmentVariable),
				typeRef(String)) [
				initializer = '''System.getenv("«environmentVariable.name»")'''
			]
		]
		if (!envParams.empty) {
			result.members += element.toMethod('checkEnvironmentVariablesOnExistence', typeRef(Void.TYPE)) [
				exceptions += typeRef(Exception)
				annotations += annotationRef('org.junit.Before') // make sure that junit is in the classpath of the workspace containing the dsl
				body = [
					val output = trace(element, true)
					envParams.forEach[generateEnvironmentVariableAssertion(output)]
				]
			]
		}

		// Create test method
		result.members += element.toMethod('execute', typeRef(Void.TYPE)) [
			exceptions += typeRef(Exception)
			annotations += annotationRef('org.junit.Test') // make sure that junit is in the classpath of the workspace containing the dsl
			body = [element.generateMethodBody(trace(element, true))]
		]
	}

	/** 
	 * Creates variables for all used fixtures minus the ones already inherited
	 * from the super class. 
	 */
	private def Iterable<JvmField> createFixtureVariables(JvmGenericType type, SetupAndCleanupProvider element) {
		val typesToInstantiate = getAllTypesToInstantiate(element, type)
		return typesToInstantiate.map [ fixtureType |
			toField(element, fixtureType.fixtureFieldName, fixtureType.typeRef) [
				if (element instanceof TestConfiguration) {
					visibility = JvmVisibility.PROTECTED
				}
				initializer = '''new «fixtureType»()'''
			]
		]
	}

	private def JvmOperation createSetupMethod(SetupAndCleanupProvider container) {
		val setup = container.setup
		return setup.toMethod(container.setupMethodName, typeRef(Void.TYPE)) [
			exceptions += typeRef(Exception)
			annotations += annotationRef('org.junit.Before')
			body = [
				val output = trace(setup, true)
				setup.contexts.forEach[generateContext(output.trace(it), #[])]
			]
		]
	}

	private def JvmOperation createCleanupMethod(SetupAndCleanupProvider container) {
		val cleanup = container.cleanup
		return cleanup.toMethod(container.cleanupMethodName, typeRef(Void.TYPE)) [
			exceptions += typeRef(Exception)
			annotations += annotationRef('org.junit.After')
			body = [
				val output = trace(cleanup, true)
				cleanup.contexts.forEach[generateContext(output.trace(it), #[])]
			]
		]
	}

	private def String getSetupMethodName(SetupAndCleanupProvider container) {
		if (container instanceof TestConfiguration) {
			return 'setup' + container.name
		}
		return 'setup'
	}

	private def String getCleanupMethodName(SetupAndCleanupProvider container) {
		if (container instanceof TestConfiguration) {
			return 'cleanup' + container.name
		}
		return 'cleanup'
	}

	def void generateMethodBody(TestCase test, ITreeAppendable output) {
		test.steps.forEach[generate(output.trace(it))]
	}

	private def void generateEnvironmentVariableAssertion(EnvironmentVariable environmentVariable,
		ITreeAppendable output) {
		output.append('''org.junit.Assert.assertNotNull(«expressionBuilder.variableToVarName(environmentVariable)»);''')
		output.newLine
	}
	
	private def void generate(SpecificationStepImplementation step, ITreeAppendable output) {
		output.appendReporterEnterCall(SemanticUnit.SPECIFICATION_STEP, step.contents.restoreString)
		step.contexts.forEach[generateContext(output.trace(it), #[])]
	}

	private def dispatch void generateContext(MacroTestStepContext context, ITreeAppendable output,
		Iterable<TestStep> macroUseStack) {
		context.steps.filter(TestStep).forEach [ step |
			output.newLine
			val macro = step.findMacroDefinition(context)
			if (macro == null) {
				logger.warn("could not find macro for test step='{}'", step.contents.restoreString)
				output.append('''// TODO Macro could not be resolved from «context.macroCollection.name»''').newLine
			} else {
				logger.debug("found macro='{}' in collection='{}' for test step='{}'", macro.name,
					context.macroCollection.name, step.contents.restoreString)
				output.append('''// Macro start: «context.macroCollection.name» - «macro.template.normalize»''').newLine
				macro.contexts.forEach [
					generateContext(output.trace(it), #[step] + macroUseStack)
				]
				output.newLine
				output.append('''// Macro end: «context.macroCollection.name» - «macro.template.normalize»''').newLine
			}
		]
	}

	private def dispatch void generateContext(ComponentTestStepContext context, ITreeAppendable output,
		Iterable<TestStep> macroUseStack) {
		output.appendReporterEnterCall(SemanticUnit.COMPONENT, context.component.name)
		context.steps.forEach[generate(output.trace(it), macroUseStack)]
	}

	protected def void generate(AbstractTestStep step, ITreeAppendable output, Iterable<TestStep> macroUseStack) {
		output.newLine
		toUnitTestCodeLine(step, output, macroUseStack)
	}

	/**
	 * @return all {@link JvmType} of all fixtures that are referenced.
	 */
	private def Set<JvmType> getFixtureTypes(SetupAndCleanupProvider element) {
		val contexts = newLinkedList
		if (element instanceof TestCase) {
			contexts += element.steps.map[it.contexts].flatten.filterNull
		}
		if (element.setup !== null) {
			contexts += element.setup.contexts
		}
		if (element.cleanup !== null) {
			contexts += element.cleanup.contexts
		}
		return contexts.map[testStepFixtureTypes].flatten.toSet
	}

	private def dispatch Set<JvmType> getTestStepFixtureTypes(ComponentTestStepContext context) {
		val interactionTypes = getAllInteractionTypes(context.component).toSet
		val fixtureTypes = interactionTypes.map[fixtureType].filterNull.toSet
		return fixtureTypes
	}

	private def dispatch Set<JvmType> getTestStepFixtureTypes(MacroTestStepContext context) {
		context.steps.filter(TestStep).map [
			val macro = findMacroDefinition(context)
			if (macro !== null) {
				return macro.contexts.filterNull.map[testStepFixtureTypes].flatten
			} else {
				return #{}
			}
		].flatten.toSet
	}

	private def String getFixtureFieldName(JvmType fixtureType) {
		return fixtureType.simpleName.toFirstLower
	}

	private def dispatch void toUnitTestCodeLine(AssertionTestStep step, ITreeAppendable output,
		Iterable<TestStep> macroUseStack) {
		macroCallVariableResolver.macroUseStack = macroUseStack
		if (step.assertExpression !== null) {
			logger.debug("generating code line for assertion test step with expression.")
			output.appendReporterEnterCall(SemanticUnit.STEP, '''assert «assertCallBuilder.assertionText(step.assertExpression)»''')
			output.append(assertCallBuilder.build(macroCallVariableResolver, step.assertExpression))
		} else if (step.testStep !== null) {
			logger.debug("generating code line for assertion test step with call to fixture.")
			val testStepRestoredString = '''«if(step.negated){"! "}else{""}»«step.testStep.contents.restoreString»'''

			val interaction = step.testStep.interaction
			if (interaction !== null) {
				val callCode = generateInteractionCall(interaction, step.testStep, macroUseStack)
				if (callCode !== null) {
					output.appendReporterEnterCall(SemanticUnit.STEP, '''assert «testStepRestoredString»''')
					val assertMethodCall = '''org.junit.Assert.assert«if(step.negated){"False"}else{"True"}»'''
					output.append('''«assertMethodCall»("«StringEscapeUtils.escapeJava(testStepRestoredString)»", «callCode»);''')
				} else {
					logger.error("Code generation failed for interaction='{}' within assert.", interaction)
					output.append('''// TODO: code generation failure for interaction='«interaction.name»' within assert (does not have a proper method reference).''')
				}
			} else {
				logger.error("Interaction not found for fixture call within assert.")
				output.append('''// TODO: code generation failure for assert failed, since expected interaction is null.''')
			}
		} else {
			throw new RuntimeException("Neither assertion expression nor fixture call is given in assertion.")
		}
	}

	private def String generateInteractionCall(InteractionType interaction, TestStep step, Iterable<TestStep> macroUseStack) {
		val fixtureField = interaction.defaultMethod?.typeReference?.type?.fixtureFieldName
		val operation = interaction.defaultMethod?.operation
		if (fixtureField !== null && operation !== null) {
			return '''«fixtureField».«operation.simpleName»(«getParameterList(step, interaction, macroUseStack)»)'''
		}
		return null
	}
	
	private def dispatch void toUnitTestCodeLine(TestStep step, ITreeAppendable output, Iterable<TestStep> macroUseStack) {
		val stepLog = step.contents.restoreString
		logger.debug("generating code line for test step='{}'.", stepLog)
		val interaction = step.interaction
		if (interaction !== null) {
			logger.debug("derived interaction with method='{}' for test step='{}'.", interaction.defaultMethod?.operation?.qualifiedName, stepLog)
			val callCode=generateInteractionCall(interaction, step, macroUseStack)
			if (callCode !== null) {
				val operation = interaction.defaultMethod?.operation
				step.maybeCreateAssignment(operation, output, stepLog)
				output.trace(interaction.defaultMethod) => [
					append('''«callCode»;'''.toString) // please call with string, since tests checks against expected string which fails for passing ''' directly
				]
			} else {
				output.append('''// TODO code generation failed for interaction='«interaction.name»' (does not have a proper method reference)''')
			}
		} else if (step.componentContext != null) {
			output.append('''// TODO could not resolve '«step.componentContext.component.name»' - «stepLog»''')
		} else {
			output.append('''// TODO could not resolve unknown component - «stepLog»''')
		}
	}
	
	private def void maybeCreateAssignment(TestStep step, JvmOperation operation, ITreeAppendable output, String stepLog) {
		if (step instanceof TestStepWithAssignment) {
			output.trace(step, TEST_STEP_WITH_ASSIGNMENT__VARIABLE, 0) => [
				// TODO should we use output.declareVariable here?
				// val variableName = output.declareVariable(step.variableName, step.variableName)
				val partialCodeLine = '''«operation.returnType.identifier» «step.variable.name» = '''
				output.appendReporterEnterCall(SemanticUnit.STEP, '''«partialCodeLine.trim» «stepLog.trim»''')
				output.append(partialCodeLine) // please call with string, since tests checks against expected string which fails for passing ''' directly
			]
		} else {
			output.appendReporterEnterCall(SemanticUnit.STEP, stepLog.trim)
		}
	}

	// TODO we could also trace the parameters here
	private def String getParameterList(TestStep step, InteractionType interaction, Iterable<TestStep> macroUseStack) {
		val mapping = getVariableToValueMapping(step, interaction.template)
		val stepContents = interaction.defaultMethod.parameters.map [ templateVariable |
			val stepContent = mapping.get(templateVariable)
			val stepContentResolved = if (stepContent instanceof VariableReference) {
					macroCallVariableResolver.macroUseStack = macroUseStack
					macroCallVariableResolver.resolveVariableReference(stepContent)
				} else {
					stepContent
				}
			return stepContentResolved
		]
		val typedValues = newArrayList
		stepContents.forEach [ stepContent, i |
			val jvmParameter = interaction.getTypeOfFixtureParameter(i)
			logger.debug("interaction='{}' has type='{}' in parameter position='{}'",
				interaction.defaultMethod.operation.qualifiedName, jvmParameter.qualifiedName, i)
			typedValues +=
				stepContent.generateCallParameters(jvmParameter, interaction, macroUseStack)
		]
		return typedValues.join(', ')
	}

	/**
	 * generate the parameter-code passed to the fixture call depending on the type of the step content
	 */
	private def dispatch Iterable<String> generateCallParameters(StepContentElement stepContent,
		JvmTypeReference expectedType, InteractionType interaction, Iterable<TestStep> macroUseStack) {
		val element = stepContent.componentElement
		val locator = '''"«element.locator»"'''
		if (interaction.defaultMethod.locatorStrategyParameters.size > 0) {
			// use element locator strategy if present, else use default of interaction
			logger.debug("resolved interaction='{}' to expect locator strategy for parameter='{}'",
				interaction.defaultMethod.operation.qualifiedName, stepContent.value)
 			val locatorStrategy = element.locatorStrategy ?: interaction.locatorStrategy
			return #[locator, locatorStrategy.qualifiedName] // locatorStrategy is the parameter right after locator (convention)
		} else {
			return #[locator]
		}
	}

	/**
	 * generate the parameter-code passed to the fixture call depending on the type of the step content
	 */
	private def dispatch Iterable<String> generateCallParameters(StepContentValue stepContentValue,
		JvmTypeReference expectedType, InteractionType interaction, Iterable<TestStep> macroUseStack) {
		if (expectedType.qualifiedName == String.name) {
			return #['''"«StringEscapeUtils.escapeJava(stepContentValue.value)»"''']
		} else {
			return #[stepContentValue.value]
		}
	}

	/**
	 * generate the parameter-code passed to the fixture call depending on the type of the step content
	 */
	private def dispatch Iterable<String> generateCallParameters(VariableReference variableReference,
		JvmTypeReference expectedType, InteractionType interaction, Iterable<TestStep> macroUseStack) {
		macroCallVariableResolver.macroUseStack = macroUseStack
		expressionBuilder.variableResolver = macroCallVariableResolver
		val result = expressionBuilder.buildExpression(variableReference)
		logger.debug("resolved variable='{}' to result='{}'", variableReference.variable.name, result)
		val isMapAccessReference = variableReference instanceof VariableReferenceMapAccess
		val expectedTypeIsString = expectedType.qualifiedName == String.canonicalName
		if (isMapAccessReference && expectedTypeIsString) {
			// convention: within a map there are only strings! there is no deep structured map!
			logger.debug("resolved variable='{}' to be a map and usage expects type String",
				variableReference.variable.name)
			return #[result + '.toString()'] // since the map is generic the actual type is java.lang.Object (thus toString) 
		} else {
			return #[result]
		}
	}

	private def String reporterFieldName() {
		val result = AbstractTestCase.declaredFields.filter [
			TestRunReporter.isAssignableFrom(type)
		].map[name].head

		if (result.nullOrEmpty) {
			throw new RuntimeException('''cannot find field of type='«TestRunReporter.name»' within test class='«AbstractTestCase.name»'.''')
		}

		return result
	}

	private def void appendReporterEnterCall(ITreeAppendable output, SemanticUnit unit, String message) {
		testRunReporterGenerator.appendReporterEnterCall(output, _typeReferenceBuilder, unit, message,
			reporterFieldName)
	}
	
}
