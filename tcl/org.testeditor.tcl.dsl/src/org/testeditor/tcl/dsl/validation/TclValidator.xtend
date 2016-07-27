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
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.Template
import org.testeditor.aml.TemplateVariable
import org.testeditor.dsl.common.util.CollectionUtils
import org.testeditor.tcl.AEVariableReference
import org.testeditor.tcl.AssertionExpression
import org.testeditor.tcl.AssertionTestStep
import org.testeditor.tcl.BinaryAssertionExpression
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.Macro
import org.testeditor.tcl.MacroCollection
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.StepContentVariableReference
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TclPackage
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.TestStepWithAssignment
import org.testeditor.tcl.impl.AssertionTestStepImpl
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tsl.SpecificationStep
import org.testeditor.tsl.StepContent
import org.testeditor.tsl.StepContentVariable
import org.testeditor.tsl.TslPackage

class TclValidator extends AbstractTclValidator {

	public static val NO_VALID_IMPLEMENTATION = 'noValidImplementation'
	public static val INVALID_NAME = 'invalidName'
	public static val INVALID_TYPED_VAR_DEREF = "invalidTypeOfVariableDereference"

	public static val UNKNOWN_NAME = 'unknownName'
	public static val INVALID_MAP_REF = 'invalidMapReference'
	public static val VARIABLE_UNKNOWN_HERE = 'varUnknownHere'
	public static val VARIABLE_ASSIGNED_MORE_THAN_ONCE = 'varAssignedMoreThanOnce'
	public static val UNALLOWED_VALUE = 'unallowedValue'
	public static val MISSING_FIXTURE = 'missingFixture'
	public static val MISSING_MACRO = 'missingMacro'
	public static val INVALID_VAR_DEREF = "invalidVariableDereference"
	public static val INVALID_MODEL_CONTENT = "invalidModelContent"

	@Inject extension TclModelUtil
	@Inject extension CollectionUtils
	@Inject ModelUtil amlModelUtil

	private static val ERROR_MESSAGE_FOR_INVALID_VAR_REFERENCE = "Dereferenced variable must be a required environment variable or a previously assigned variable"

	@Check
	def void tclHasOnlyTestCases(TclModel tclModel) {
		val fileExtension = tclModel.eResource.URI.fileExtension
		switch (fileExtension) {
			case "tcl":
				if (tclModel.macroCollection != null) {
					error("this file type may only contain test cases but it contains macro definitions",
						tclModel.macroCollection, null)
				}
			case "tml":
				if (tclModel.test != null) {
					error("this file type may only contain macro definitions but it contains test cases",
						tclModel.test, null)
				}
			default:
				throw new RuntimeException('''unknown file extension (fileExtensions='«fileExtension»')''')
		}
	}

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
	def void checkAllReferencedVariablesAreKnown(TestStepContext context, Set<String> knownVariableNames,
		String errorMessage) {
		switch context {
			ComponentTestStepContext: {
				val completedKnownVariableNames = newHashSet
				completedKnownVariableNames.addAll(knownVariableNames)
				context.steps.forEach [ step, index |
					step.checkAllReferencedVariablesAreKnown(completedKnownVariableNames, errorMessage)
					val declaredVariables = step.collectDeclaredVariablesTypeMap.keySet
					completedKnownVariableNames.addAll(declaredVariables) // complete list of known variables with the ones declared in this very step
				]
			}
			MacroTestStepContext:
				context.step.checkAllReferencedVariablesAreKnown(knownVariableNames, errorMessage)
			default:
				throw new RuntimeException('''Unknown TestStepContextType '«context.class.canonicalName»'.''')
		}
	}

	/**
	 * check that each variable usage/deref is a known variable
	 */
	private def checkAllReferencedVariablesAreKnown(TestStep step, Set<String> knownVariableNames,
		String errorMessage) {
		// contents are indexed so that errors can be set to the precise location (index within the contents)
		val erroneousIndexedStepContents = step.contents.indexed.filterValue(StepContentVariableReference).filter [
			!knownVariableNames.contains(value.variable.name)
		]
		erroneousIndexedStepContents.forEach [
			error(errorMessage, value.eContainer, value.eContainingFeature, key, INVALID_VAR_DEREF)
		]
	}

	/** 
	 * get the actual jvm types from the fixtures that are transitively used and to which this variable/parameter is passed to.
	 * Since a parameter can be used in multiple parameter positions of subsequent fixture calls, the size of the set of types can be > 1
	 */
	def dispatch Set<JvmTypeReference> getAllTypeUsagesOfVariable(MacroTestStepContext callingMacroTestStepContext,
		String variable) {
		val macroCalled = callingMacroTestStepContext.findMacroDefinition
		if (macroCalled != null) {
			val templateParamToVarRefMap = mapCalledTemplateParamToCallingVariableReference(
				callingMacroTestStepContext.step, macroCalled.template, variable)
			val calledMacroTemplateParameters = templateParamToVarRefMap.keySet.map[name].toSet
			val contextsUsingAnyOfTheseParameters = macroCalled.contexts.filter [
				makesUseOfVariablesViaReference(calledMacroTemplateParameters)
			]
			val typesOfAllParametersUsed = contextsUsingAnyOfTheseParameters.map [ context |
				context.getAllTypeUsagesOfVariables(calledMacroTemplateParameters)
			].flatten.toSet
			return typesOfAllParametersUsed
		} else {
			return #{}
		}
	}

	def Iterable<JvmTypeReference> getAllTypeUsagesOfVariables(TestStepContext context, Iterable<String> variables) {
		variables.map [ parameter |
			context.getAllTypeUsagesOfVariable(parameter)
		].flatten
	}

	def Map<TemplateVariable, StepContent> mapCalledTemplateParamToCallingVariableReference(TestStep callingStep,
		Template calledMacroTemplate, String callingVariableReference) {
		val varMap = getVariableToValueMapping(callingStep, calledMacroTemplate)
		return varMap.filter [ key, stepContent |
			stepContent.makesUseOfVariablesViaReference(#{callingVariableReference})
		]
	}

	/** 
	 * get the actual jvm types from the fixtures that are transitively used and to which this variable/parameter is passed to
	 */
	def dispatch Set<JvmTypeReference> getAllTypeUsagesOfVariable(ComponentTestStepContext componentTestStepContext,
		String variableName) {
		val stepsUsingThisVariable = componentTestStepContext.steps.filter [
			contents.filter(StepContentVariableReference).exists[variable.name == variableName]
		]
		val typesUsages = stepsUsingThisVariable.map [ step |
			step.stepVariableFixtureParameterTypePairs.filterKey(StepContentVariableReference).filter [
				key.variable.name == variableName
			].map[value]
		].flatten.filterNull.toSet
		return typesUsages
	}

	/**
	 * does the given context make use of (one of the) variables passed via variable reference?
	 */
	private def boolean makesUseOfVariablesViaReference(TestStepContext context, Set<String> variables) {
		switch context {
			ComponentTestStepContext: context.steps.exists[contents.exists[makesUseOfVariablesViaReference(variables)]]
			MacroTestStepContext: context.step.contents.exists[makesUseOfVariablesViaReference(variables)]
			default: false
		}
	}

	/**
	 * does the given step make use of (one of the) variables passed via variable reference?
	 */
	private def boolean makesUseOfVariablesViaReference(StepContent stepContent, Set<String> variables) {
		if (stepContent instanceof StepContentVariableReference) {
			return variables.contains(stepContent.variable.name)
		}
		return false
	}

	@Check
	def void checkVariableUsageWithinAssertionExpressions(Macro macro) {
		val Map<String, String> declaredVariablesTypeMap = newHashMap
		macro.contexts.forEach[executeCheckVariableUsageWithinAssertionExpressions(declaredVariablesTypeMap)]
	}

	def dispatch void executeCheckVariableUsageWithinAssertionExpressions(
		ComponentTestStepContext componentTestStepContext, Map<String, String> declaredVariablesTypeMap) {
		executeTestStepCheckVariableUsageWithinAssertionExpressions(componentTestStepContext.steps, declaredVariablesTypeMap)
	}

	def dispatch void executeCheckVariableUsageWithinAssertionExpressions(MacroTestStepContext macroTestStepContext,
		Map<String, String> declaredVariablesTypeMap) {
		executeTestStepCheckVariableUsageWithinAssertionExpressions(#[macroTestStepContext.step], declaredVariablesTypeMap)
	}

	private def void executeTestStepCheckVariableUsageWithinAssertionExpressions(Iterable<TestStep> steps,
		Map<String, String> declaredVariablesTypeMap) {
		steps.forEach [ it, index |
			if (it instanceof TestStepWithAssignment) {
				// check "in order" (to prevent variable usage before assignment)
				if (declaredVariablesTypeMap.containsKey(variable.name)) {
					val message = '''Variable '«variable.name»' is assigned more than once.'''
					error(message, it, TclPackage.Literals.TEST_STEP_WITH_ASSIGNMENT__VARIABLE, index,
						VARIABLE_ASSIGNED_MORE_THAN_ONCE);
				} else {
					declaredVariablesTypeMap.put(variable.name, interaction.defaultMethod.operation.returnType.identifier)
				}
			} else if (it instanceof AssertionTestStepImpl) {
				executeCheckVariableUsageWithinAssertionExpressions(declaredVariablesTypeMap, index)
			}
		]
	}

	private def executeCheckVariableUsageWithinAssertionExpressions(AssertionTestStep step,
		Map<String, String> declaredVariablesTypeMap, int index) {
		step.expression.collectVariableUsage.forEach [
			if (!declaredVariablesTypeMap.containsKey(variable.name)) { // regular variable dereference
				val message = '''Variable «if(variable.name!=null){ '\''+variable.name+'\''}» is unknown here.'''
				error(message, eContainer, eContainingFeature, VARIABLE_UNKNOWN_HERE)
			} else if (key != null) { // dereference map with a key
				val typeIdentifier = declaredVariablesTypeMap.get(variable.name).replaceFirst("<.*", "") // remove generics
				if (typeIdentifier.isNotAssignableToMap) {
					val message = '''Variable '«variable.name»' of type '«typeIdentifier»' does not implement '«Map.canonicalName»'. It cannot be used with key '«key»'.'''
					error(message, eContainer, eContainingFeature, INVALID_MAP_REF)
				}
			}
		]
	}
	
	private def Map<String, JvmTypeReference> collectDeclaredVariablesTypeMap(TestStepContext context){
		switch(context){
			ComponentTestStepContext: {
				val result=newHashMap
				context.steps.map[collectDeclaredVariablesTypeMap].forEach[result.putAll(it)]
				return result
				}
			MacroTestStepContext: return context.step.collectDeclaredVariablesTypeMap
			default: throw new RuntimeException('''unknown test step context «context.class.canonicalName»''')
		}
	}
	private def Map<String, JvmTypeReference> collectDeclaredVariablesTypeMap(TestStep testStep) {
		if(testStep instanceof TestStepWithAssignment){
			val typeReference = testStep.interaction?.defaultMethod?.operation?.returnType			
			return #{ testStep.variable.name -> typeReference}
		}
		return emptyMap
	}

	private def isNotAssignableToMap(String typeIdentifier) {
		return !typeof(Map).isAssignableFrom(Class.forName(typeIdentifier))
	}

	private def Iterable<AEVariableReference> collectVariableUsage(AssertionExpression expression) {
		switch (expression) {
			BinaryAssertionExpression:
				return expression.left.collectVariableUsage + expression.right.collectVariableUsage
			AEVariableReference:
				return #[expression]
			default:
				return #[]
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
	def void checkVariableUsageWithinAssertionExpressions(TclModel tclModel) {
		val Map<String,String> varTypeMap = newHashMap
		tclModel.test.steps.map[contexts].flatten.forEach[executeCheckVariableUsageWithinAssertionExpressions(varTypeMap)]
	}

	private def boolean matches(List<SpecificationStep> specSteps,
		List<SpecificationStepImplementation> specImplSteps) {
		if (specSteps.size > specImplSteps.size) {
			return false
		}
		return specImplSteps.map[contents.restoreString].containsAll(specSteps.map[contents.restoreString])
	}

	@Check
	def void checkName(TestCase testCase) {
		val expectedName = testCase.expectedName
		if (testCase.name != expectedName) {
			val message = '''Test case name='«testCase.name»' does not match expected name='«expectedName»' based on filename='«testCase.model.eResource.URI.lastSegment»'.'''
			error(message, TclPackage.Literals.TEST_CASE__NAME, INVALID_NAME)
		}
	}

	@Check
	def void checkName(MacroCollection macroCollection) {
		val expectedName = macroCollection.expectedName
		if (macroCollection.name != expectedName) {
			val message = '''Macro collection name='«macroCollection.name»' does not match expected name='«expectedName»' based on  filename='«macroCollection.model.eResource.URI.lastSegment»'.'''
			error(message, TclPackage.Literals.MACRO_COLLECTION__NAME, INVALID_NAME)
		}
	}

	@Check
	def void checkVariableUsage(TestCase testCase){
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
	def void checkVariableUsage(Macro macro){
		// each macro opens its own scope, so the macro knows about the ones
		// introduced by the parameters as defined by the template 
		// and the ones introduced by assignments within the macro itself (which are successively added)
		val macroParameterNames=amlModelUtil.getReferenceableVariables(macro.template).map[name]
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
	def void checkVariableUsageIsWellTyped(MacroCollection macroCollection) {
		macroCollection.macros.forEach [
			// NOTE: all variable references to macro parameters are excluded from this type check
			// the check whether a macro parameter is used in a type correct manner is checked by 'checkMacroParameterUsage'			
			// each macro opens its own scope, so the macro knows about required variables and the ones 
			// introduced by assignments within the macro itself
			val macroParameterNames = amlModelUtil.getReferenceableVariables(template).map[name].toSet
			val knownVariablesTypeMapWithinMacro = newHashMap
			contexts.forEach[knownVariablesTypeMapWithinMacro.putAll(collectDeclaredVariablesTypeMap)]
			contexts.forEach [
				checkReferencedVariablesAreUsedWellTypedExcluding(knownVariablesTypeMapWithinMacro, macroParameterNames)
			]
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
		switch ctx {
			ComponentTestStepContext: ctx.steps.forEach [
				checkReferencedVariablesAreUsedWellTypedExcluding(declaredVariablesTypeMap, ctx, excludedVariableNames)
			]
			MacroTestStepContext: ctx.step.
				checkReferencedVariablesAreUsedWellTypedExcluding(declaredVariablesTypeMap, ctx, excludedVariableNames)
			default: throw new RuntimeException('''Unknown TestStepContextType '«ctx.class.canonicalName»'.''')
		}
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
	private def void checkReferencedVariablesAreUsedWellTypedExcluding(TestStep step,
		Map<String, JvmTypeReference> declaredVariablesTypeMap, TestStepContext context, Set<String> excludedVariableNames) {
		// build this index to be able to correctly issue an error on the element by index
		val variablesIndexed = step.contents.indexed.filter [
			value instanceof StepContentVariableReference || value instanceof StepContentVariable ||
				value instanceof StepContentElement
		]
		val variableReferences = variablesIndexed.filterValue(StepContentVariableReference).filter [
			!excludedVariableNames.contains(value.variable.name)
		]
		variableReferences.forEach [
			val varName = value.variable.name
			val typeUsageSet = context.getAllTypeUsagesOfVariable(varName).filterNull.toSet
			val typeDeclared = declaredVariablesTypeMap.get(varName)
			// currently this is a naiive check, expecting the types
			if (typeDeclared==null || !typeUsageSet.identicalSingleTypeInSet(typeDeclared)) {
				error('''Variable='«varName»' is declared to be of type='«typeDeclared?.qualifiedName»' but is used in a position that expects type(s)='«typeUsageSet.map[qualifiedName].join(", ")»'.''',
					value.eContainer, value.eContainingFeature, key, INVALID_TYPED_VAR_DEREF)
			}
		]
	}
	
	/**
	 * both sets hold only one type and this type is equal
	 */
	private def boolean identicalSingleTypeInSet(Set<JvmTypeReference> typeSet, JvmTypeReference type) {
		typeSet.size == 1 && typeSet.head.qualifiedName == type.qualifiedName
	}

}
