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

import java.util.List
import java.util.Map
import java.util.Optional
import java.util.Set
import javax.inject.Inject
import org.apache.commons.lang3.StringEscapeUtils
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
import org.testeditor.tcl.AssignmentVariable
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
import org.testeditor.tcl.VariableReferenceMapAccess
import org.testeditor.tcl.dsl.jvmmodel.SimpleTypeComputer
import org.testeditor.tcl.dsl.jvmmodel.TclExpressionTypeComputer
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
	public static val INVALID_MAP_ACCESS = 'invalidMapAccess'
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
	@Inject extension TclTypeValidationUtil
	@Inject extension ModelUtil
	@Inject extension CollectionUtils
	
	@Inject ValueSpaceHelper valueSpaceHelper
	@Inject AmlValidator amlValidator
	@Inject SimpleTypeComputer simpleTypeComputer
	@Inject TclExpressionTypeComputer expressionTypeComputer

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
				info("test step could not resolve fixture", TclPackage.Literals.TEST_STEP__FIXTURE_REFERENCE, MISSING_FIXTURE)
			}
		}
	}

	@Check
	def checkMacroCall(TestStep testStep) {
		if (testStep.hasMacroContext) {
			val normalizedTeststep = testStep.normalize
			val macroCollection = testStep.macroContext.macroCollection
			if (!macroCollection.macros.exists[template.normalize == normalizedTeststep]) {
				warning("test step could not resolve macro usage", TclPackage.Literals.TEST_STEP__FIXTURE_REFERENCE,
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
					val declaredVariables = simpleTypeComputer.collectDeclaredVariablesTypeMap(step).keySet
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

	/**
	 * check that each variable usage/deref is a known variable
	 */
	private def dispatch void checkAllReferencedVariablesAreKnown(TestStep step, Set<String> knownVariableNames,
		String errorMessage) {
		// contents are indexed so that errors can be set to the precise location (index within the contents)
		val erroneousIndexedStepContents = step.fixtureReference.contents.indexed.filterValue(VariableReference).filter [
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

	private def isAssignableToMap(JvmTypeReference type) {
		if (type === null) {
			return false
		}
		try {
			val qualifiedTypeNameWithoutGenerics = type.qualifiedName.replaceFirst("<.*", "")
			return typeof(Map).isAssignableFrom(Class.forName(qualifiedTypeNameWithoutGenerics))
		} catch (ClassNotFoundException e) {
			return false
		}
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
			declaredVariableNames.addAll(simpleTypeComputer.collectDeclaredVariablesTypeMap(it).keySet)
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
			declaredVariableNames.addAll(simpleTypeComputer.collectDeclaredVariablesTypeMap(it).keySet)
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
		macro.contexts.forEach[knownVariablesTypeMapWithinMacro.putAll(simpleTypeComputer.collectDeclaredVariablesTypeMap(it))]
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
			forEach[knownVariablesTypeMap.putAll(simpleTypeComputer.collectDeclaredVariablesTypeMap(it))]
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
					val coercedType = expressionTypeComputer.coercedTypeOfComparison(comparison)
					if (coercedType === null || !coercedType.isOrderable) {
						error('Sorry, comparing order of non numeric values is not supported, yet.', comparison.eContainer, comparison.eContainingFeature, INVALID_ORDER_TYPE)
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
	 */
	private def dispatch void checkReferencedVariablesAreUsedWellTypedExcluding(TestStep step,
		Map<String, JvmTypeReference> declaredVariablesTypeMap, TestStepContext context,
		Set<String> excludedVariableNames) {
		// build this variables index to be able to correctly issue an error on the element by index
		val variablesIndexed = step.fixtureReference.contents.indexed.filter [!(value instanceof StepContentText)]
		val variableReferencesIndexed = variablesIndexed.filterValue(VariableReference).filter [
			!excludedVariableNames.contains(value.variable.name)
		]
		variableReferencesIndexed.forEach [
			val varName = value.variable.name
			val typeUsageSet = context.getAllTypeUsagesOfVariable(varName).filterNull.toSet
			val typeDeclared = declaredVariablesTypeMap.get(varName)
			checkVariableReferenceIsWellTyped(value, typeUsageSet, typeDeclared, varName, key)
		]
	}
	
	private def dispatch void checkReferencedVariablesAreUsedWellTypedExcluding(AssertionTestStep step,
		Map<String, JvmTypeReference> declaredVariablesTypeMap, TestStepContext context,
		Set<String> excludedVariableNames) {
		val variableReferences = step.assertExpression.eAllContents.filter(VariableReference)			
		variableReferences.forEach [
			val varName = variable.name
			val typeUsageSet = context.getAllTypeUsagesOfVariable(varName).filterNull.toSet
			val typeDeclared = declaredVariablesTypeMap.get(varName)
			checkVariableReferenceIsWellTyped(typeUsageSet, typeDeclared, varName, 0)
		]
	}
	
	private def checkVariableReferenceIsWellTyped(VariableReference variableReference, Set<Optional<JvmTypeReference>> typeUsageSet, JvmTypeReference typeDeclared, String variableName, Integer errorIndex) {
		if (variableReference.variable instanceof TemplateVariable) {
			// do not type check variables that are passed via parameters (are templateVariables),
			// since they are typeless until actually used by a call, 
			// but then these variables are either assignmentVariables or environmentVariables
			return
		}
		switch variableReference {
			VariableReferenceMapAccess:
				// do no type checking on values retrieved from a map, but check whether this is actually a map
				if (!typeDeclared.assignableToMap) {
					error('''Variable='«variableName»' is declared to be of type='«typeDeclared?.qualifiedName»' but is used in a position that expects type(s)='«Map.canonicalName»'.''',
						variableReference.eContainer, variableReference.eContainingFeature, errorIndex,
						INVALID_MAP_ACCESS)
				}
			VariableReference: {
				// currently this is a naive check, expecting the types
				// TODO typeUsageSet is empty for assertions because TclModelUtil.getAllTypeUsagesOfVariable is not implemented for them
				val coercibleTypeNames = #[String, long, boolean, Boolean].map[name]
				val setOfTypeNamesUsed = typeUsageSet.filter[present].map[get.type.qualifiedName]
				val typeUsageSetContainsOnlyCoercibleTypes = setOfTypeNamesUsed.filter [!coercibleTypeNames.contains(it)].empty
				val coercionPossible = typeDeclared.qualifiedName.equals(String.name) && typeUsageSetContainsOnlyCoercibleTypes
				val typesMatch = doTypesMatch(typeDeclared,typeUsageSet)
				if (!coercionPossible && !typesMatch) {
					error('''Variable='«variableName»' is declared to be of type='«typeDeclared?.qualifiedName»' but is used in a position that expects type(s)='«typeUsageSet.filter[present].map[get.qualifiedName].join(", ")»'.''',
						variableReference.eContainer, variableReference.eContainingFeature, errorIndex,
						INVALID_TYPED_VAR_DEREF)
					}
				}
			default:
				throw new RuntimeException('''Unknown variable reference type='«variableReference.class.canonicalName»'.''')
		}
	}
	
	private def boolean doTypesMatch(JvmTypeReference typeDeclared, Set<Optional<JvmTypeReference>> typeUsageSet) {
		return typeDeclared !== null && (typeUsageSet.isEmpty || typeUsageSet.filter[present].map [
			get.qualifiedNameWithoutGenerics
		].toSet.identicalSingleTypeInSet(typeDeclared.qualifiedNameWithoutGenerics))
	}
	
	private def String getQualifiedNameWithoutGenerics(JvmTypeReference typeReference) {
		return typeReference.qualifiedName.replaceFirst("<.*", "")
	}
	
	/**
	 * both sets hold only one type and this type is equal
	 */
	private def boolean identicalSingleTypeInSet(Set<String> qualifiedTypeNameSet, String qualifiedTypeName) {
		qualifiedTypeNameSet.size == 1 && qualifiedTypeNameSet.head == qualifiedTypeName
	}

	private def boolean matches(List<SpecificationStep> specSteps,
		List<SpecificationStepImplementation> specImplSteps) {
		if (specSteps.size > specImplSteps.size) {
			return false
		}
		return specImplSteps.map[contents.restoreString].containsAll(specSteps.map[contents.restoreString])
	}

	private def boolean isOrderable(JvmTypeReference typeReference) {
		switch (typeReference.qualifiedName) {
			case long.name, 
			case Long.name : return true
			default: return false
		}
	}
	
	private def void checkStepContentVariableTypeInParameterPosition(TestStep step, InteractionType interaction) {
		// check only StepContentVariable, since variable references are already tested by ...
		val callParameters=step.fixtureReference.contents.indexed.filterValue(StepContentVariable)
		val definitionParameterTypePairs = simpleTypeComputer.getVariablesWithTypes(interaction)		
		callParameters.forEach [ contentIndexPair |
			val content = contentIndexPair.value
			val contentIndex = contentIndexPair.key
			val templateParameter = content.templateParameterForCallingStepContent
			val expectedType = definitionParameterTypePairs.get(templateParameter)
			if (!expectedType.present) {
				error('Type unknown. Expected \'' + expectedType + '\'.',
					content.eContainer, content.eContainingFeature, contentIndex, INVALID_PARAMETER_TYPE)
			} else {
				val expectedTypeQualName = expectedType.get.qualifiedName
				if (expectedTypeQualName.equals(long.name)) {
					content.checkThatUseAsTypedLongIsOk(contentIndex)
				}
				if (expectedTypeQualName.equals(boolean.name) || expectedTypeQualName.equals(Boolean.name)) {
					content.checkThatUseAsTypedBooleanIsOk(contentIndex)
				}
			}
		]		
	}
	
	private def void checkStepContentVariableTypeInParameterPosition(TestStep step, Macro macro) {
		// check only StepContentVariable, since variable references are already tested by ...
		val templateParameterTypeMap = simpleTypeComputer.getVariablesWithTypes(macro)
		val contentTemplateVarmap = step.getStepContentToTemplateVariablesMapping(macro.template)
		contentTemplateVarmap.filterKey(StepContentVariable).forEach [ content, templateVar |
			val expectedType = templateParameterTypeMap.get(templateVar)
			expectedType.ifPresent [
				val contentIndex = step.fixtureReference.contents.indexOfFirst(content)
				val expectedTypeQualName = expectedType.get.qualifiedName
				if (expectedTypeQualName.equals(long.name)) {
					content.checkThatUseAsTypedLongIsOk(contentIndex)
				}
				if (expectedTypeQualName.equals(boolean.name) || expectedTypeQualName.equals(Boolean.name)) {
					content.checkThatUseAsTypedBooleanIsOk(contentIndex)
				}
			]
		]
	}

	private def void checkThatUseAsTypedLongIsOk(StepContent content, int contentIndex) {
		switch (content) {
			StepContentVariable:
				try {
					Integer.parseInt(content.value)
				} catch (NumberFormatException nfe) {
					error('''Type mismatch. Expected 'long' got 'String' = '«StringEscapeUtils.escapeJava(content.value)»' that cannot be converted to long.''',
						content.eContainer, content.eContainingFeature, contentIndex, INVALID_PARAMETER_TYPE)
				}
			default: checkThatUseAsValueIsOk(content, contentIndex)				
		}
	}
	
	private def void checkThatUseAsValueIsOk(StepContent content, int contentIndex) {
		switch (content) {
			StepContentVariable: {
				// is a value => is allowed
			}
			VariableReferenceMapAccess: {
				// allowed, must be here before 'VariableReference' because it's a subtype
			}
			VariableReference: 			
				if (content.variable instanceof AssignmentVariable) {
					val varType = (content.variable as AssignmentVariable).determineType
					val qualifiedName = varType.qualifiedName
					switch(qualifiedName) {
						case long.name: { /* is ok */ }
						case String.name: { /* is ok */ }
						case boolean.name: { /* is ok */ }
						case Boolean.name: { /* is ok */ }
						default: {
						error('''Type mismatch. Expected a value (e.g. boolean, long, String) but got '«qualifiedName»'.''', content.eContainer,
							content.eContainingFeature, contentIndex, INVALID_PARAMETER_TYPE)
						}
					}
				}
		}
	}

	private def void checkThatUseAsTypedBooleanIsOk(StepContent content, int contentIndex) {
		switch (content) {
			StepContentVariable:
				if(content.value != Boolean.toString(true) && content.value != Boolean.toString(false)) {
					error('''Type mismatch. Expected 'boolean' got 'String' = '«StringEscapeUtils.escapeJava(content.value)»' that cannot be converted to boolean.''',
						content.eContainer, content.eContainingFeature, contentIndex, INVALID_PARAMETER_TYPE)
				}
			default: checkThatUseAsValueIsOk(content, contentIndex)				
		}
	}
	
}
