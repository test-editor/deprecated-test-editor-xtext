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
package org.testeditor.tcl.dsl.validation

import com.google.gson.JsonObject
import java.util.List
import java.util.Map
import java.util.Optional
import java.util.Set
import javax.inject.Inject
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.validation.Check
import org.eclipse.xtext.xtype.XImportSection
import org.testeditor.aml.InteractionType
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.Template
import org.testeditor.aml.TemplateVariable
import org.testeditor.aml.dsl.validation.AmlValidator
import org.testeditor.dsl.common.util.CollectionUtils
import org.testeditor.tcl.AssertionTestStep
import org.testeditor.tcl.AssignmentThroughPath
import org.testeditor.tcl.ComparatorGreaterThan
import org.testeditor.tcl.ComparatorLessThan
import org.testeditor.tcl.Comparison
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.Macro
import org.testeditor.tcl.MacroCollection
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.TclPackage
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.VariableReferencePathAccess
import org.testeditor.tcl.dsl.jvmmodel.SimpleTypeComputer
import org.testeditor.tcl.dsl.jvmmodel.TclCoercionComputer
import org.testeditor.tcl.dsl.jvmmodel.TclExpressionTypeComputer
import org.testeditor.tcl.dsl.jvmmodel.TclJvmTypeReferenceUtil
import org.testeditor.tcl.dsl.jvmmodel.TclTypeUsageComputer
import org.testeditor.tcl.dsl.jvmmodel.VariableCollector
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tcl.util.ValueSpaceHelper
import org.testeditor.tsl.SpecificationStep
import org.testeditor.tsl.StepContent
import org.testeditor.tsl.StepContentText
import org.testeditor.tsl.StepContentVariable
import org.testeditor.tsl.TslPackage

import static org.testeditor.dsl.common.CommonPackage.Literals.*

class TclValidator extends AbstractTclValidator {

	public static val NO_VALID_IMPLEMENTATION = 'noValidImplementation'
	public static val INVALID_NAME = 'invalidName'
	public static val INVALID_TYPED_VAR_DEREF = "invalidTypeOfVariableDereference"

	public static val UNKNOWN_NAME = 'unknownName'
	public static val INVALID_JSON_ACCESS = 'invalidJsonAccess'
	public static val VARIABLE_UNKNOWN_HERE = 'varUnknownHere'
	public static val VARIABLE_ASSIGNED_MORE_THAN_ONCE = 'varAssignedMoreThanOnce'
	public static val UNALLOWED_VALUE = 'unallowedValue'
	public static val MISSING_FIXTURE = 'missingFixture'
	public static val MISSING_MACRO = 'missingMacro'
	public static val INVALID_VAR_DEREF = "invalidVariableDereference"
	public static val INVALID_MODEL_CONTENT = "invalidModelContent"
	public static val INVALID_PARAMETER_TYPE = "invalidParameterType"
	public static val INVALID_ORDER_TYPE = "invalidOrderType"

	@Inject extension TclModelUtil
	@Inject extension ModelUtil
	@Inject extension CollectionUtils
	
	@Inject ValueSpaceHelper valueSpaceHelper
	@Inject AmlValidator amlValidator
	@Inject SimpleTypeComputer simpleTypeComputer
	@Inject TclExpressionTypeComputer expressionTypeComputer
	@Inject VariableCollector variableCollector
	@Inject TclTypeUsageComputer typeUsageComputer
	@Inject TclJvmTypeReferenceUtil typeReferenceUtil
	@Inject TclCoercionComputer coercionComputer

	private static val ERROR_MESSAGE_FOR_INVALID_VAR_REFERENCE = "Dereferenced variable must be a required environment variable or a previously assigned variable"
	
	@Check
	def void referencesComponentElement(StepContentElement contentElement) {
		val component = contentElement.componentElement
		if (component === null) {
			error('No ComponentElement found.', contentElement, null)
		}
	}

	override checkImports(XImportSection importSection) {
		// ignore for now
	}

	@Check
	def checkMaskPresent(ComponentTestStepContext tsContext) {
		if (tsContext.component.eIsProxy) {
			warning("component/mask is not defined in aml", TclPackage.Literals.COMPONENT_TEST_STEP_CONTEXT__COMPONENT,
				UNKNOWN_NAME)
		}
	}

	@Check
	def checkFixtureMethodForExistence(TestStep testStep) {
		if (!(testStep instanceof AssertionTestStep) && testStep.hasComponentContext) {
			val method = testStep.interaction?.defaultMethod
			if ((method == null ) || (method.operation == null) || (method.typeReference?.type == null)) {
				info("test step could not resolve fixture", TclPackage.Literals.TEST_STEP__CONTENTS, MISSING_FIXTURE)
			}
		}
	}

	@Check
	def checkMacroCall(TestStep testStep) {
		if (testStep.hasMacroContext) {
			val normalizedTeststep = testStep.normalize
			val macroCollection = testStep.macroContext.macroCollection
			if (!macroCollection.macros.exists[template.normalize == normalizedTeststep]) {
				warning("test step could not resolve macro usage", TclPackage.Literals.TEST_STEP__CONTENTS,
					MISSING_MACRO)
			}
		}
	}

	/**
	 *  check that each variable reference used is known, adding to the known variables all 
	 *  the ones that are declared (e.g. through assignment) within the steps 'visited'
	 */
	private def dispatch void checkAllReferencedVariablesAreKnown(TestStepContext context, Set<String> knownVariableNames,
		String errorMessage) {
		switch context {
			ComponentTestStepContext: {
				val completedKnownVariableNames = newHashSet
				completedKnownVariableNames.addAll(knownVariableNames)
				context.steps.forEach [ step, index |
					step.checkAllReferencedVariablesAreKnown(completedKnownVariableNames, errorMessage)
					val declaredVariables = variableCollector.collectDeclaredVariablesTypeMap(step).keySet
					val alreadyKnown=declaredVariables.filter[completedKnownVariableNames.contains(it)]
					if(!alreadyKnown.empty) {
						error('''The variable(s)='«alreadyKnown.join(',')»' is (are) already known''', step, null, VARIABLE_ASSIGNED_MORE_THAN_ONCE)
					}
					completedKnownVariableNames.addAll(declaredVariables) // complete list of known variables with the ones declared in this very step
				]
			}
			MacroTestStepContext:
				context.steps.forEach[checkAllReferencedVariablesAreKnown(knownVariableNames, errorMessage)]
			default:
				throw new RuntimeException('''Unknown TestStepContextType '«context.class.canonicalName»'.''')
		}
	}

	private def dispatch void checkAllReferencedVariablesAreKnown(AssignmentThroughPath assignment, Set<String> knownVariableNames,
		String errorMessage) {
		val erroneousContents = assignment.eAllContents.filter(VariableReference).filter [
			!knownVariableNames.contains(variable.name)
		]
		erroneousContents.forEach [
			error(errorMessage, assignment.eContainer, assignment.eContainingFeature, INVALID_VAR_DEREF)
		]
	}

	/**
	 * check that each variable usage/deref is a known variable
	 */
	private def dispatch void checkAllReferencedVariablesAreKnown(TestStep step, Set<String> knownVariableNames,
		String errorMessage) {
		// contents are indexed so that errors can be set to the precise location (index within the contents)
		val erroneousIndexedStepContents = step.contents.indexed.filterValue(VariableReference).filter [
			!knownVariableNames.contains(value.variable.name)
		]
		erroneousIndexedStepContents.forEach [
			error(errorMessage, value.eContainer, value.eContainingFeature, key, INVALID_VAR_DEREF)
		]
	}

	private def dispatch void checkAllReferencedVariablesAreKnown(AssertionTestStep step, Set<String> knownVariableNames,
		String errorMessage) {
		val erroneousContents = step.assertExpression.eAllContents.filter(VariableReference).filter [
			!knownVariableNames.contains(variable.name)
		]
		erroneousContents.forEach [
			error(errorMessage, step.assertExpression.eContainer, step.assertExpression.eContainingFeature, INVALID_VAR_DEREF)
		]		
	}

	@Check
	def void checkValueInValueSpace(StepContentVariable stepContentVariable) {
		val valueSpace = valueSpaceHelper.getValueSpace(stepContentVariable)
		if (valueSpace.present && !valueSpace.get.isValidValue(stepContentVariable.value)) {
			val message = '''Value is not allowed in this step. Allowed values: '«valueSpace.get»'.'''
			warning(message, TslPackage.Literals.STEP_CONTENT_VALUE__VALUE, UNALLOWED_VALUE);
		}
	}

	@Check
	def void checkSpec(TestCase testCase) {
		val specification = testCase.specification
		if (specification != null) {
			if (!specification.steps.matches(testCase.steps)) {
				val message = '''Test case does not implement its specification '«specification.name»'.'''
				warning(message, TclPackage.Literals.TEST_CASE__SPECIFICATION, NO_VALID_IMPLEMENTATION)
			}
		}
	}

	@Check
	def void checkName(TestCase testCase) {
		val expectedName = testCase.expectedName
		if (testCase.name != expectedName) {
			val message = '''Test case name='«testCase.name»' does not match expected name='«expectedName»' based on filename='«testCase.model.eResource.URI.lastSegment»'.'''
			error(message, NAMED_ELEMENT__NAME, INVALID_NAME)
		}
	}

	@Check
	def void checkName(MacroCollection macroCollection) {
		val expectedName = macroCollection.expectedName
		if (macroCollection.name != expectedName) {
			val message = '''Macro collection name='«macroCollection.name»' does not match expected name='«expectedName»' based on  filename='«macroCollection.model.eResource.URI.lastSegment»'.'''
			error(message, NAMED_ELEMENT__NAME, INVALID_NAME)
		}
	}

	@Check
	def void checkVariableUsage(TestCase testCase) {
		val initiallyDeclaredVariableNames = testCase.model.envParams.map[name]
		val declaredVariableNames = newHashSet
		declaredVariableNames.addAll(initiallyDeclaredVariableNames)
		testCase.steps.map[contexts].flatten.forEach [
			checkAllReferencedVariablesAreKnown(declaredVariableNames, ERROR_MESSAGE_FOR_INVALID_VAR_REFERENCE)
			// add the variables declared by this step to be known for subsequent steps
			declaredVariableNames.addAll(variableCollector.collectDeclaredVariablesTypeMap(it).keySet)
		]
	}
	
	@Check
	def void checkVariableUsage(Macro macro) {
		// each macro opens its own scope, so the macro knows about the ones
		// introduced by the parameters as defined by the template 
		// and the ones introduced by assignments within the macro itself (which are successively added)
		val macroParameterNames = if (macro.template === null) {
				emptySet
			} else {
				amlModelUtil.getReferenceableVariables(macro.template).map[name]
			} 
		val declaredVariableNames = newHashSet
		declaredVariableNames.addAll(macroParameterNames)
		macro.contexts.forEach [
			checkAllReferencedVariablesAreKnown(declaredVariableNames, ERROR_MESSAGE_FOR_INVALID_VAR_REFERENCE)
			// add the variables declared by this step to be known for subsequent steps within this macro !! 
			declaredVariableNames.addAll(variableCollector.collectDeclaredVariablesTypeMap(it).keySet)
		]
	}

	/** 
	 * Check that all variables are used such that  
	 * their types match the expectation (e.g. of the fixture transitively called) 
	 */
	@Check
	def void checkVariableUsageIsWellTyped(Macro macro) {
		// NOTE: all variable references to macro parameters are excluded from this type check
		// the check whether a macro parameter is used in a type correct manner is checked by 'checkMacroParameterUsage'			
		// each macro opens its own scope, so the macro knows about required variables and the ones 
		// introduced by assignments within the macro itself (=> no check on macro collection necessary)
		val macroParameterNames = if (macro.template === null) {
				emptySet
			} else {
				amlModelUtil.getReferenceableVariables(macro.template).map[name].toSet
			} 
		val knownVariablesTypeMapWithinMacro = newHashMap
		macro.contexts.forEach[knownVariablesTypeMapWithinMacro.putAll(variableCollector.collectDeclaredVariablesTypeMap(it))]
		macro.contexts.forEach [
			checkReferencedVariablesAreUsedWellTypedExcluding(knownVariablesTypeMapWithinMacro, macroParameterNames)
		]
	}
	
	/** 
	 * Check that all variables are used such that  
	 * their types match the expectation (e.g. of the fixture transitively called) 
	 */
	@Check
	def void checkVariableUsageIsWellTyped(TestCase testCase) {
		val knownVariablesTypeMap = testCase.model.envParams.environmentVariablesTypeMap
		testCase.steps.map[contexts].flatten => [
			forEach[knownVariablesTypeMap.putAll(variableCollector.collectDeclaredVariablesTypeMap(it))]
			forEach[checkAllReferencedVariablesAreUsedWellTyped(knownVariablesTypeMap)]
		]
	}

	/**
	 * check that comparisons that need orderable (numeric) types are ok (use numeric types)
	 */
	@Check
	def void checkNumericWhenCheckingOrder(Comparison comparison) {
		if (comparison.comparator !== null && comparison.left !== null && comparison.right !== null) {
			switch (comparison.comparator) {
				ComparatorGreaterThan,
				ComparatorLessThan: {
					typeReferenceUtil.initWith(comparison.eResource)
					val coercedType = expressionTypeComputer.coercedTypeOfComparison(comparison, null)
					if (coercedType === null || !typeReferenceUtil.isOrderable(coercedType)) {
						error('''Sorry, comparing order of non numeric values (coerced type='«coercedType?.qualifiedName»') is not supported, yet.''', comparison.eContainer, comparison.eContainingFeature, INVALID_ORDER_TYPE)
					}
				}
			}
		}
	}
	
	@Check
	def void checkTemplateHoldsValidCharacters(Template template) {
		amlValidator.checkTemplateHoldsValidCharacters(template)
	}

	@Check
	def void checkStepParameterTypes(TestStep step) {
		switch step {
			case step.hasComponentContext: {
				val interaction = step.interaction
				if (interaction !== null) {
					checkStepContentVariableTypeInParameterPosition(step, interaction)
				}
			}
			case step.hasMacroContext: {
				val macro = step.findMacroDefinition(step.macroContext)
				if (macro !== null) {
					checkStepContentVariableTypeInParameterPosition(step, macro)
				}
			}
			default: throw new RuntimeException("TestStep has unknown context (neither component nor macro).")
		}
	}
	
	/**
	 * check that all (except the explicitly excluded) variables are used according to their actual type (transitively in their fixture)
	 */
	private def void checkReferencedVariablesAreUsedWellTypedExcluding(TestStepContext ctx,
		Map<String, JvmTypeReference> declaredVariablesTypeMap, Set<String> excludedVariableNames) {
		ctx.steps.forEach [
			checkReferencedVariablesAreUsedWellTypedExcluding(declaredVariablesTypeMap, ctx, excludedVariableNames)
		]
	}

	/**
	 * check that all variables are used according to their actual type (transitively in their fixture)
	 */
	private def void checkAllReferencedVariablesAreUsedWellTyped(TestStepContext ctx,
		Map<String, JvmTypeReference> declaredVariablesTypeMap) {
		checkReferencedVariablesAreUsedWellTypedExcluding(ctx, declaredVariablesTypeMap, #{})
	}

	/**
	 * check that all variables are used according to their actual type (transitively in their fixture)
	 * excluding variables in the excludedVariableNames (e.g. macro parameters, since they are untyped until used)
	 */
	private def dispatch void checkReferencedVariablesAreUsedWellTypedExcluding(TestStep step,
		Map<String, JvmTypeReference> declaredVariablesTypeMap, TestStepContext context,
		Set<String> excludedVariableNames) {
		// build this variables index to be able to correctly issue an error on the element by index
		val variablesIndexed = step.contents.indexed.filter [!(value instanceof StepContentText)]
		val variableReferencesIndexed = variablesIndexed.filterValue(VariableReference).filter [
			!excludedVariableNames.contains(value.variable.name)
		]
		variableReferencesIndexed.forEach [
			checkVariableReferenceIsUsedWellTyped(value, declaredVariablesTypeMap, context, key)
		]
	}

	private def dispatch void checkReferencedVariablesAreUsedWellTypedExcluding(AssignmentThroughPath assignment,
		Map<String, JvmTypeReference> declaredVariablesTypeMap, TestStepContext context,
		Set<String> excludedVariableNames) {
		assignment.eAllContents.filter(VariableReference).filter [
			!excludedVariableNames.contains(variable.name)
		].forEach [
			checkVariableReferenceIsUsedWellTyped(declaredVariablesTypeMap, context, 0)
		]
	}
	
	private def dispatch void checkReferencedVariablesAreUsedWellTypedExcluding(AssertionTestStep step,
		Map<String, JvmTypeReference> declaredVariablesTypeMap, TestStepContext context,
		Set<String> excludedVariableNames) {
		step.assertExpression.eAllContents.filter(VariableReference).filter [
			!excludedVariableNames.contains(variable.name)
		].forEach [
			checkVariableReferenceIsUsedWellTyped(declaredVariablesTypeMap, context, 0)
		]
	}
	
	/**
	 * check that this variableReference is used according to its (expected) type
	 */
	private def void checkVariableReferenceIsUsedWellTyped(VariableReference variableReference,
		Map<String, JvmTypeReference> declaredVariablesTypeMap, TestStepContext context, int errorReportingIndex) {
		val varName = variableReference.variable.name
		val typeUsageSet = typeUsageComputer.getAllPossibleTypeUsagesOfVariable(context, varName).filterNull.toSet
		val typeDeclared = declaredVariablesTypeMap.get(varName)
		checkVariableReferenceIsWellTyped(variableReference, typeUsageSet, typeDeclared, errorReportingIndex)
	}
	
	/** 
	 * check whether the given variableReference is used according to its (expected) type
	 */
	private def checkVariableReferenceIsWellTyped(VariableReference variableReference,
		Set<Optional<JvmTypeReference>> typeUsageSet, JvmTypeReference typeDeclared, int errorReportingIndex) {
		if (variableReference.variable instanceof TemplateVariable) {
			// do not type check variables that are passed via parameters (are templateVariables),
			// since they are typeless until actually used by a call, 
			// but then these variables are either assignmentVariables or environmentVariables
			return
		}
		typeReferenceUtil.initWith(variableReference.eResource)
		coercionComputer.initWith(variableReference.eResource)
		switch variableReference {
			VariableReferencePathAccess:
				// must be Json
				if (!typeReferenceUtil.isJson(typeDeclared)) {
					error('''Variable='«variableReference.variable.name»' is declared to be of type='«typeDeclared?.qualifiedName»' but is used in a position that expects a json type (e.g. «JsonObject.name»).''',
						variableReference.eContainer, variableReference.eContainingFeature, errorReportingIndex,
						INVALID_JSON_ACCESS)
				}
			VariableReference: {
				val illegalTypeUsages = typeUsageSet.filter[present].filter [
					!typeReferenceUtil.isAssignableFrom(get, typeDeclared) &&
						!coercionComputer.isCoercionPossible(get, typeDeclared)
				]
				if (!illegalTypeUsages.empty) {
					error('''Variable='«variableReference.variable.name»' is declared to be of type='«typeDeclared?.qualifiedName»' but is used in context(s) expecting type(s)='«typeUsageSet.filter[present].map[get.qualifiedName].join(", ")»' of which the types = '«illegalTypeUsages.filter[present].map[get.qualifiedName].join(", ")»' are problematic (non assignable nor coercible). Please make sure that no conflicting type usages remain.''',
						variableReference.eContainer, variableReference.eContainingFeature, errorReportingIndex,
						INVALID_TYPED_VAR_DEREF)
					}
				}
			default:
				throw new RuntimeException('''Unknown variable reference type='«variableReference.class.canonicalName»'.''')
		}
	}
	
	private def boolean matches(List<SpecificationStep> specSteps,
		List<SpecificationStepImplementation> specImplSteps) {
		if (specSteps.size > specImplSteps.size) {
			return false
		}
		return specImplSteps.map[contents.restoreString].containsAll(specSteps.map[contents.restoreString])
	}

	/**
	 * check that the given test step uses parameters that can be used for calls to the interaction 
	 * (are typed accordingly, or can be coerced accordingly)
	 */
	private def void checkStepContentVariableTypeInParameterPosition(TestStep step, InteractionType interaction) {
		// check only StepContentVariable, since variable references are already tested by ...
		val callParameters = step.contents.indexed.filterValue(StepContentVariable)
		val definitionParameterTypePairs = simpleTypeComputer.getVariablesWithTypes(interaction)
		callParameters.forEach [ contentIndexPair |
			val content = contentIndexPair.value
			val contentIndex = contentIndexPair.key
			val templateParameter = content.templateParameterForCallingStepContent
			val expectedType = definitionParameterTypePairs.get(templateParameter)
			if (!expectedType.present) {
				error('''Expected type could not be found for templateParameter = '«templateParameter?.name»'.''', content.eContainer,
					content.eContainingFeature, contentIndex, INVALID_PARAMETER_TYPE)
			} else {
				checkAgainstTypeExpectation(content, contentIndex, expectedType.get)
			}
		]
	}
	
	/**
	 * check that the given test step uses parameters that can be used for calls to the macro 
	 * (are typed accordingly, or can be coerced accordingly)
	 */
	private def void checkStepContentVariableTypeInParameterPosition(TestStep step, Macro macro) {
		// check only StepContentVariable, since variable references are already tested by ...
		val templateParameterTypeMap = simpleTypeComputer.getVariablesWithTypes(macro)
		val contentTemplateVarmap = step.getStepContentToTemplateVariablesMapping(macro.template)
		contentTemplateVarmap.filterKey(StepContentVariable).forEach [ content, templateVar |
			val expectedType = templateParameterTypeMap.get(templateVar)
			val contentIndex = step.contents.indexOfFirst(content)
			if (!expectedType.present) {
				error('''Expected type could not be found for templateVariable = '«templateVar?.name»'.''', content.eContainer,
					content.eContainingFeature, contentIndex, INVALID_PARAMETER_TYPE)
			} else {
				checkAgainstTypeExpectation(content, contentIndex, expectedType.get)
			}
		]
	}
	
	/** 
	 * check that the given step content can be assigned / coerced to the expected type.
	 * coercion is not checked for StepContentVariables (since these are constants which are already checked 
	 * for matching the expected type by the 'determineType' function).
	 */
	private def void checkAgainstTypeExpectation(StepContent content, int contentIndex, JvmTypeReference expectedType) {
		typeReferenceUtil.initWith(content.eResource)
		coercionComputer.initWith(content.eResource)
		val contentType = expressionTypeComputer.determineType(content, Optional.of(expectedType))
		val assignable = typeReferenceUtil.isAssignableFrom(expectedType, contentType)
		// if content is a StepContentVariable, coercion is not option, since determineType already did check for bool/long/string
		val coercible = !(content instanceof StepContentVariable) &&
			coercionComputer.isCoercionPossible(expectedType, contentType)
		if (!assignable && !coercible) {
			error('''Type mismatch. Expected '«expectedType.qualifiedName»' got '«contentType.qualifiedName»' that cannot assigned nor coerced.''',
				content.eContainer, content.eContainingFeature, contentIndex, INVALID_PARAMETER_TYPE)
		}
	}
		
}
