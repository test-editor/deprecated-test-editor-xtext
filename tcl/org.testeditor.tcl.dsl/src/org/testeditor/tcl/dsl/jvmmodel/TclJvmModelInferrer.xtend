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

import com.google.common.annotations.VisibleForTesting
import java.util.List
import java.util.Optional
import java.util.Set
import javax.inject.Inject
import org.apache.commons.lang3.StringEscapeUtils
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.common.types.JvmConstructor
import org.eclipse.xtext.common.types.JvmDeclaredType
import org.eclipse.xtext.common.types.JvmField
import org.eclipse.xtext.common.types.JvmGenericType
import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.common.types.JvmType
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.common.types.JvmVisibility
import org.eclipse.xtext.generator.trace.ILocationData
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.serializer.ISerializer
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable
import org.eclipse.xtext.xbase.jvmmodel.AbstractModelInferrer
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder
import org.slf4j.LoggerFactory
import org.testeditor.aml.InteractionType
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.TemplateContainer
import org.testeditor.aml.TemplateVariable
import org.testeditor.fixture.core.AbstractTestCase
import org.testeditor.fixture.core.TestRunReportable
import org.testeditor.fixture.core.TestRunReporter
import org.testeditor.fixture.core.TestRunReporter.SemanticUnit
import org.testeditor.tcl.AbstractTestStep
import org.testeditor.tcl.AssertionTestStep
import org.testeditor.tcl.AssignmentThroughPath
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.EnvironmentVariable
import org.testeditor.tcl.JsonValue
import org.testeditor.tcl.Macro
import org.testeditor.tcl.MacroCollection
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
import org.testeditor.tcl.VariableReferencePathAccess
import org.testeditor.tcl.dsl.jvmmodel.macro.MacroHelper
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tsl.StepContent
import org.testeditor.tsl.StepContentValue

import static org.testeditor.tcl.TclPackage.Literals.*
import org.testeditor.dsl.common.util.JvmTypeReferenceUtil

class TclJvmModelInferrer extends AbstractModelInferrer {

	static val logger = LoggerFactory.getLogger(TclJvmModelInferrer)

	@Inject extension JvmTypesBuilder
	@Inject extension ModelUtil
	@Inject extension TclModelUtil
	@Inject TclAssertCallBuilder assertCallBuilder
	@Inject IQualifiedNameProvider nameProvider
	@Inject JvmModelHelper jvmModelHelper
	@Inject TclExpressionBuilder expressionBuilder
	@Inject TestRunReporterGenerator testRunReporterGenerator 
	@Inject MacroHelper macroHelper
	@Inject SimpleTypeComputer typeComputer
	@Inject TclExpressionTypeComputer expressionTypeComputer
	@Inject ISerializer serializer 
	@Inject TclJsonUtil jsonUtil
	@Inject TclCoercionComputer coercionComputer
	@Inject JvmTypeReferenceUtil typeReferenceUtil
		
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
	
	@VisibleForTesting
	def void initWith(ResourceSet resourceSet) {
		coercionComputer.initWith(resourceSet)
		typeReferenceUtil.initWith(resourceSet)
	}

	/**
	 * First perform common operations like variable initialization and then dispatch to subclass specific operations.
	 */
	def dispatch void infer(SetupAndCleanupProvider element, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		// Create the class with eager initialization
		val generatedClass = element.toClass(isPreIndexingPhase)

		// Stuff that can be done in late initialization
		acceptor.accept(generatedClass) [
			initWith(element.eResource.resourceSet)
			
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

	private def boolean implementsInterface(JvmDeclaredType type, Class<?> ^interface) {
		val interfaceName=^interface.canonicalName
		return type.extendedInterfaces.map[qualifiedName].exists[equals(interfaceName)] //
		|| { // lazy or -> recursion only if necessary
			val parent = type.extendedClass?.type
			if (parent !== null && parent instanceof JvmDeclaredType) {
				(parent as JvmDeclaredType).implementsInterface(^interface)
			} else {
				false
			}
		}
	}

	private def Iterable<JvmDeclaredType> getAllInstantiatedTypesImplementingTestRunReportable(
		SetupAndCleanupProvider element, JvmDeclaredType generatedClass) {
		return getAllTypesToInstantiate(element, generatedClass) //
		.filter(JvmDeclaredType) //
		.filter[implementsInterface(TestRunReportable)]
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

		// Create methods for macros
		val macros = macroHelper.getAllTransitiveMacros(element)
		result.members += macros.map [ macro |
			element.toMethod(macroHelper.getMethodName(macro), typeRef(Void.TYPE)) [
				exceptions += typeRef(Exception)
				visibility = JvmVisibility.PRIVATE
				val variablesWithTypes = typeComputer.getVariablesWithTypes(macro)
				parameters += variablesWithTypes.entrySet.map[toParameter(key, key.name, value.orElse(typeRef(String)))]
				body = [macro.generateMethodBody(trace(macro))]
			]
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
				setup.contexts.forEach[generateContext(output.trace(it))]
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
				cleanup.contexts.forEach[generateContext(output.trace(it))]
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

	def void generateMethodBody(Macro macro, ITreeAppendable output) {
		macro.contexts.forEach[generateContext(output.trace(it))]
	}

	private def void generateEnvironmentVariableAssertion(EnvironmentVariable environmentVariable,
		ITreeAppendable output) {
		val varName = expressionBuilder.variableToVarName(environmentVariable)
		output.append('''org.junit.Assert.assertNotNull("environment variable '«environmentVariable.name»' must not be null", «varName»);''')
		output.newLine
	}
	
	private def void generate(SpecificationStepImplementation step, ITreeAppendable output) {
		output.appendReporterEnterCall(SemanticUnit.SPECIFICATION_STEP, step.contents.restoreString)
		step.contexts.forEach[generateContext(output.trace(it))]
	}

	private def dispatch void generateContext(MacroTestStepContext context, ITreeAppendable output) {
		output.newLine
		output.append('''// Macro: «context.macroCollection.name»''')
		context.steps.filter(TestStep).forEach[generateMacroCall(context, output.trace(it))]
	}

	private def dispatch void generateContext(ComponentTestStepContext context, ITreeAppendable output) {
		output.appendReporterEnterCall(SemanticUnit.COMPONENT, context.component.name)
		context.steps.forEach[generate(output.trace(it))]
	}

	protected def void generate(AbstractTestStep step, ITreeAppendable output) {
		output.newLine
		toUnitTestCodeLine(step, output)
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

	private def dispatch void toUnitTestCodeLine(AssertionTestStep step, ITreeAppendable output) {
		val locationString = '''«output.traceRegion.associatedSrcRelativePath.toString»:«output.traceRegion.associatedLocations.map[toReportableString].join(", ")»'''
		logger.debug('''generating code line for assertion test step at «locationString».''')
		output.appendReporterEnterCall(SemanticUnit.STEP, '''assert «assertCallBuilder.assertionText(step.assertExpression)»''')
		output.append(assertCallBuilder.build(step.assertExpression, locationString))
	}
	
	private def String toReportableString(ILocationData locationData) {
		if (locationData.lineNumber == locationData.endLineNumber) {
			return Integer.toString(locationData.lineNumber + 1)
		} else {
			return '''«locationData.lineNumber + 1»-«locationData.endLineNumber + 1»'''.toString
		}
	}
	
	private def dispatch void toUnitTestCodeLine(AssignmentThroughPath step, ITreeAppendable output) {
		val varType = expressionTypeComputer.determineType(step.variableReference.variable, Optional.empty)
		if (jsonUtil.isJsonType(varType)) {
			toUnitTestCodeLineOfJsonAssignment(step, output)
		} else {
			throw new RuntimeException('''Unknown variable type = '«varType?.qualifiedName»' for assignment with key path''')
		}
	}
	
	private def void toUnitTestCodeLineOfJsonAssignment(AssignmentThroughPath step, ITreeAppendable output) {
		val varRef = step.getVariableReference

		val stepLog = '''«varRef?.restoreString» = «assertCallBuilder.assertionText(step.expression)»'''
		logger.debug("generating code line for test step='{}'.", stepLog)
		output.appendReporterEnterCall(SemanticUnit.STEP, '''«stepLog»''')

		val expression = step.expression.actualMostSpecific
		switch expression {
			JsonValue: {
				val parsedValue = jsonUtil.jsonParseInstruction('''"«StringEscapeUtils.escapeJava(serializer.serialize(expression).trim)»"''')
				val code = '''«expressionBuilder.buildWriteExpression(varRef, parsedValue)»;'''
				output.append(code)
			}
			VariableReferencePathAccess,
			VariableReference: {
				val valueString = expressionBuilder.buildReadExpression(step.expression)
				val variableType = expressionTypeComputer.determineType(expression, Optional.empty)
				if (jsonUtil.isJsonType(variableType)) {
					val code = '''«expressionBuilder.buildWriteExpression(varRef, valueString)»;'''
					output.append(code)
				} else {
					val parsedValue = coercionComputer.generateCoercion(typeReferenceUtil.jsonElementJvmTypeReference, variableType, valueString)
					val code = '''«expressionBuilder.buildWriteExpression(varRef, parsedValue)»;'''
					output.append(code)
				}
			}
			default: throw new RuntimeException('''Cannot generate code for expression of type='«step.expression»' within assignments to json variables.''')
		}
	}
	
	private def dispatch void toUnitTestCodeLine(TestStep step, ITreeAppendable output) {
		val stepLog = step.contents.restoreString
		logger.debug("generating code line for test step='{}'.", stepLog)
		val interaction = step.interaction
   		
		if (interaction !== null) {
			logger.debug("derived interaction with method='{}' for test step='{}'.",
				interaction.defaultMethod?.operation?.qualifiedName, stepLog)
			val fixtureField = interaction.defaultMethod?.typeReference?.type?.fixtureFieldName
			val operation = interaction.defaultMethod?.operation
			if (fixtureField !== null && operation !== null) {
				step.maybeCreateAssignment(operation, output, stepLog)
				output.trace(interaction.defaultMethod) => [
					step.contents.filter(VariableReference).forEach [
						val expectedType = typeComputer.getExpectedType(it, interaction)
						val variableType = expressionTypeComputer.determineType(variable, expectedType?:Optional.empty)
						if (coercionComputer.isTypeCoercionPossible(expectedType.get, variableType)) {
							val coercionCheck = generateCoercionCheck(interaction, expectedType?:Optional.empty)
							if (!coercionCheck.nullOrEmpty) {
								output.append(coercionCheck).newLine
							}
						}
					]
					val codeLine = '''«fixtureField».«operation.simpleName»(«generateCallParameters(step, interaction)»);'''
					output.append(codeLine) // please call with string, since tests checks against expected string which fails for passing ''' directly
				]
			} else {
				output.append('''// TODO interaction type '«interaction.name»' does not have a proper method reference''')
			}
		} else if (step.componentContext != null) {
			logger.debug("interaction not found within context of component '{}' for test step='{}'.", step.componentContext.component.name, stepLog)
			output.append('''org.junit.Assert.fail("Template '«StringEscapeUtils.escapeJava(stepLog)»' cannot be resolved with any known macro/fixture. Please check your «step.locationInfo»");''')
		} else {
			logger.debug("interaction not found in unknown context for test step='{}'.", stepLog)
			output.append('''org.junit.Assert.fail("Template '«StringEscapeUtils.escapeJava(stepLog)»' cannot be resolved with any known macro/fixture. Please check your  Macro-, Config- or Testcase-File ");''')
		}
	}

	private def String generateCoercionCheck(VariableReference variableReference, TemplateContainer templateContainer,
		Optional<JvmTypeReference> expectedType) {
		val variableAccessCode = toParameterString(variableReference, expectedType, templateContainer, false).head
		val varRefType = expressionTypeComputer.determineType(variableReference.variable, expectedType?:Optional.empty)
		val quotedErrorMessage = '''"Parameter is expected to be of type = '«expectedType.get.qualifiedName»' but a non coercible value = '"+«variableAccessCode».toString()+"' was passed through variable reference = '«variableReference.variable.name»'."'''
		return coercionComputer.generateCoercionGuard(expectedType.get, varRefType, variableAccessCode,
			quotedErrorMessage)
	}
	
	private def String getLocationInfo(TestStep step) {
		val lineNumber = NodeModelUtils.findActualNodeFor(step).startLine
		
		val testCase = EcoreUtil2.getContainerOfType(step, TestCase)
		if (testCase !== null) {
			return '''Testcase '«testCase.name»' in line «lineNumber».'''
		}

		val macroLib = EcoreUtil2.getContainerOfType(step, MacroCollection)
		if (macroLib !== null) {
			return '''Macro '«macroLib.name»' in line «lineNumber».''' 
		}

		val config = EcoreUtil2.getContainerOfType(step, TestConfiguration)
		if (config !== null) {
			return '''Configuration '«config.name»' in line «lineNumber».'''	
		}
		
		throw new IllegalArgumentException;
	} 

	private def void generateMacroCall(TestStep step, MacroTestStepContext context, ITreeAppendable output) {
		val stepLog = step.contents.restoreString
		logger.debug("generating code line for macro test step='{}'.", stepLog)
		output.newLine
		output.append('''// - «stepLog»''')
		output.newLine
		val macro = step.findMacroDefinition(context)
		if (macro !== null) {
			step.contents.filter(VariableReference).forEach [
				val expectedType = typeComputer.getExpectedType(it, macro)
				val variableType = expressionTypeComputer.determineType(variable, expectedType?:Optional.empty)
				if ((expectedType?:Optional.empty).present && coercionComputer.isTypeCoercionPossible(expectedType.get, variableType)) { 
					val coercionCheck = generateCoercionCheck(macro, expectedType)
					if (!coercionCheck.nullOrEmpty) {
						output.append(coercionCheck).newLine
					}
				}
			]
			val parameters = generateCallParameters(step, macro)
			output.append('''«macroHelper.getMethodName(macro)»(«parameters»);''')
		} else {
			output.append('''org.junit.Assert.fail("Could not resolve '«context.macroCollection.name»'. Please check your «step.locationInfo»");''')
		}
	}

	private def String generateCallParameters(TestStep step, TemplateContainer templateContainer) {
		val stepContentsWithTypes = getVariablesWithTypesInOrder(step, templateContainer)
		stepContentsWithTypes.map[toParameterString(key, value, templateContainer, true)].flatten.join(', ')
	}

	private def List<Pair<StepContent, Optional<JvmTypeReference>>> getVariablesWithTypesInOrder(TestStep step, TemplateContainer templateContainer) {
		val variablesWithTypes = typeComputer.getVariablesWithTypes(templateContainer)
		val stepContentToTemplateVariables = step.getStepContentToTemplateVariablesMapping(templateContainer.template)
		if (templateContainer instanceof InteractionType) {
			// need to consider the order of the parameters in the method call
			val variableToIndexMap = templateContainer.defaultMethod.parameters.indexed.toMap[value].mapValues[key]
			val sorted = stepContentToTemplateVariables.entrySet.sortBy[variableToIndexMap.get(value)]
			return sorted.map[key -> variablesWithTypes.get(value)].toList
		} else {
			// for macros the order of the appearance in the test step is fine
			return stepContentToTemplateVariables.entrySet.map[key -> variablesWithTypes.get(value)].toList
		}
	}

	/**
	 * Converts a {@link StepContent} to a parameter call. If it is a {@link VariableReference} this
	 * is straightforward - just use the variable name. If is is a {@link StepContentValue} we need
	 * to check the type of the underlying {@link TemplateVariable} - if it is a String we need to
	 * put it in quotes. If the type is not specified we assume a String to handle parameter passing gracefully.
	 */
	// TODO return type is only an iterable because of the locator strategy
	private def Iterable<String> toParameterString(StepContent stepContent, Optional<JvmTypeReference> expectedType,
		TemplateContainer templateContainer, boolean withCoercion) {
			
		// in case the parameter is an aml element, the locator + (optional) the locator strategy must be generated
		if (templateContainer instanceof InteractionType) {
			if (stepContent instanceof StepContentElement) {
				return toLocatorParameterString(stepContent, templateContainer)
			}
		}

		// in all other cases, check for necessary coercion
		val parameterString = stepContent.contentToParameterString(expectedType?:Optional.empty, templateContainer)
		// val expectedType = typeComputer.getExpectedType(stepContent, templateContainer)
		if ((expectedType?:Optional.empty).present) {
			val stepContentType = expressionTypeComputer.determineType(stepContent, expectedType)
			val coercionPossible = coercionComputer.isTypeCoercionPossible(expectedType.get, stepContentType)
			if (withCoercion && coercionPossible) {
				return #[coercionComputer.generateCoercion(expectedType.get, stepContentType, parameterString)]
			}
		}
		return #[parameterString]
	}
	
	private def dispatch String contentToParameterString(VariableReference variableReference, Optional<JvmTypeReference> parameterType,
		TemplateContainer templateContainer) {
		return expressionBuilder.buildReadExpression(variableReference)
	}
	
	private def dispatch String contentToParameterString(StepContentValue stepContentValue, Optional<JvmTypeReference> parameterType,
		TemplateContainer templateContainer) {
		val stepContentType = expressionTypeComputer.determineType(stepContentValue, parameterType?:Optional.empty)
		val contentIsANumberOrBool = typeReferenceUtil.isANumber(stepContentType) || typeReferenceUtil.isBoolean(stepContentType)
		val fixtureParameterTypeIsKnownToBeString = parameterType.present && typeReferenceUtil.isString(parameterType.get)
		if (fixtureParameterTypeIsKnownToBeString || !contentIsANumberOrBool) {
			// if fixture parameter is a string this is easy, just quote the value
			// if the content itself is not a number nor a boolean its probably a good idea to quote this value, too
			return '''"«StringEscapeUtils.escapeJava(stepContentValue.value)»"'''
		} else {
			// if content is definitely a boolean or a number and this is also expected by the called fixture,
			return stepContentValue.value
		}
	}
	
	private def Iterable<String> toLocatorParameterString(StepContentElement stepContent, InteractionType interaction) {
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

	private def String reporterFieldName() {
		val result = AbstractTestCase.declaredFields.findFirst[TestRunReporter.isAssignableFrom(type)]?.name

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