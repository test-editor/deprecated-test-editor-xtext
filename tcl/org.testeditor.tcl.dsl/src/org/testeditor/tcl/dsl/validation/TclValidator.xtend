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
import java.util.Set
import javax.inject.Inject
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.validation.Check
import org.eclipse.xtext.xtype.XImportSection
import org.testeditor.dsl.common.util.CollectionUtils
import org.testeditor.tcl.AssertionTestStep
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
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tsl.SpecificationStep
import org.testeditor.tsl.StepContentText
import org.testeditor.tsl.StepContentVariable
import org.testeditor.tsl.TslPackage

import static org.testeditor.dsl.common.CommonPackage.Literals.*
import org.testeditor.aml.TemplateVariable

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

	@Inject extension TclModelUtil
	@Inject extension CollectionUtils

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
					val declaredVariables = step.collectDeclaredVariablesTypeMap.keySet
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

	private def isNotAssignableToMap(JvmTypeReference type) {
		try {
			val qualifiedTypeNameWithoutGenerics = type.qualifiedName.replaceFirst("<.*", "")
			return !typeof(Map).isAssignableFrom(Class.forName(qualifiedTypeNameWithoutGenerics))
		} catch (ClassNotFoundException e) {
			return false
		}
	}

	@Check
	def checkValueInValueSpace(StepContentVariable stepContentVariable) {
		val valueSpace = stepContentVariable.valueSpaceAssignment?.valueSpace
		if (valueSpace !== null && !valueSpace.isValidValue(stepContentVariable.value)) {
			val message = '''Value is not allowed in this step. Allowed values: '«valueSpace»'.'''
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
			declaredVariableNames.addAll(collectDeclaredVariablesTypeMap.keySet)
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
			declaredVariableNames.addAll(collectDeclaredVariablesTypeMap.keySet)
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
		macro.contexts.forEach[knownVariablesTypeMapWithinMacro.putAll(collectDeclaredVariablesTypeMap)]
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
			forEach[knownVariablesTypeMap.putAll(collectDeclaredVariablesTypeMap)]
			forEach[checkAllReferencedVariablesAreUsedWellTyped(knownVariablesTypeMap)]
		]
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
		val variablesIndexed = step.contents.indexed.filter [!(value instanceof StepContentText)]
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
	
	private def checkVariableReferenceIsWellTyped(VariableReference variableReference, Set<JvmTypeReference> typeUsageSet, JvmTypeReference typeDeclared, String variableName, Integer errorIndex) {
		if (variableReference.variable instanceof TemplateVariable) {
			// do not type check variables that are passed via parameters (are templateVariables),
			// since they are typeless until actually used by a call, 
			// but then these variables are either assignmentVariables or environmentVariables
			return
		}
		switch variableReference {
			VariableReferenceMapAccess:
				// do no type checking on values retrieved from a map, but check whether this is actually a map
				if (typeDeclared == null || isNotAssignableToMap(typeDeclared)) {
					error('''Variable='«variableName»' is declared to be of type='«typeDeclared?.qualifiedName»' but is used in a position that expects type(s)='«Map.canonicalName»'.''',
							variableReference.eContainer, variableReference.eContainingFeature, errorIndex,
							INVALID_MAP_ACCESS)
				}
			VariableReference:
				// currently this is a naive check, expecting the types
				// TODO typeUsageSet is empty for assertions because TclModelUtil.getAllTypeUsagesOfVariable is not implemented for them
				if (typeDeclared == null || (!typeUsageSet.isEmpty && !typeUsageSet.identicalSingleTypeInSet(typeDeclared))) {
					error('''Variable='«variableName»' is declared to be of type='«typeDeclared?.qualifiedName»' but is used in a position that expects type(s)='«typeUsageSet.map[qualifiedName].join(", ")»'.''',
							variableReference.eContainer, variableReference.eContainingFeature, errorIndex,
							INVALID_TYPED_VAR_DEREF)
				}
			default:
				throw new RuntimeException('''Unknown variable reference type='«variableReference.class.canonicalName»'.''')
		}
	}
	
	/**
	 * both sets hold only one type and this type is equal
	 */
	private def boolean identicalSingleTypeInSet(Set<JvmTypeReference> typeSet, JvmTypeReference type) {
		typeSet.size == 1 && typeSet.head.qualifiedName == type.qualifiedName
	}

	private def boolean matches(List<SpecificationStep> specSteps,
		List<SpecificationStepImplementation> specImplSteps) {
		if (specSteps.size > specImplSteps.size) {
			return false
		}
		return specImplSteps.map[contents.restoreString].containsAll(specSteps.map[contents.restoreString])
	}

}
