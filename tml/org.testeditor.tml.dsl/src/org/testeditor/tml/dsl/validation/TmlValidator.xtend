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
package org.testeditor.tml.dsl.validation

import java.util.Map
import java.util.Set
import javax.inject.Inject
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.validation.Check
import org.eclipse.xtext.xtype.XImportSection
import org.testeditor.aml.Template
import org.testeditor.aml.TemplateVariable
import org.testeditor.dsl.common.util.CollectionUtils
import org.testeditor.tml.AEVariableReference
import org.testeditor.tml.AssertionExpression
import org.testeditor.tml.AssertionTestStep
import org.testeditor.tml.BinaryAssertionExpression
import org.testeditor.tml.ComponentTestStepContext
import org.testeditor.tml.Macro
import org.testeditor.tml.MacroTestStepContext
import org.testeditor.tml.StepContentElement
import org.testeditor.tml.StepContentVariableReference
import org.testeditor.tml.TestStep
import org.testeditor.tml.TestStepContext
import org.testeditor.tml.TestStepWithAssignment
import org.testeditor.tml.TmlPackage
import org.testeditor.tml.impl.AssertionTestStepImpl
import org.testeditor.tml.util.TmlModelUtil
import org.testeditor.tsl.StepContent
import org.testeditor.tsl.StepContentVariable
import org.testeditor.tsl.TslPackage

/**
 * This class contains custom validation rules. 
 * 
 * See https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#validation
 */
class TmlValidator extends AbstractTmlValidator {

	public static val UNKNOWN_NAME = 'unknownName'
	public static val INVALID_MAP_REF = 'invalidMapReference'
	public static val VARIABLE_UNKNOWN_HERE = 'varUnknownHere'
	public static val VARIABLE_ASSIGNED_MORE_THAN_ONCE = 'varAssignedMoreThanOnce'
	public static val UNALLOWED_VALUE = 'unallowedValue'
	public static val MISSING_FIXTURE = 'missingFixture'
	public static val MISSING_MACRO = 'missingMacro'
	public static val INVALID_VAR_DEREF = "invalidVariableDereference"

	@Inject extension TmlModelUtil
	@Inject extension CollectionUtils

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
			warning("component/mask is not defined in aml", TmlPackage.Literals.COMPONENT_TEST_STEP_CONTEXT__COMPONENT,
				UNKNOWN_NAME)
		}
	}

	@Check
	def checkFixtureMethodForExistence(TestStep testStep) {
		if (!(testStep instanceof AssertionTestStep) && testStep.hasComponentContext) {
			val method = testStep.interaction?.defaultMethod
			if ((method == null ) || (method.operation == null) || (method.typeReference?.type == null)) {
				info("test step could not resolve fixture", TmlPackage.Literals.TEST_STEP__CONTENTS, MISSING_FIXTURE)
			}
		}
	}

	@Check
	def checkMacroCall(TestStep testStep) {
		if (testStep.hasMacroContext) {
			val normalizedTeststep = testStep.normalize
			val macroCollection = testStep.macroContext.macroCollection
			if (!macroCollection.macros.exists[template.normalize == normalizedTeststep]) {
				warning("test step could not resolve macro usage", TmlPackage.Literals.TEST_STEP__CONTENTS,
					MISSING_MACRO)
			}
		}
	}

	@Check
	def checkMacroParameterUsage(Macro macro) {
		val templateParameterNames = macro.template.contents.filter(TemplateVariable).map[name].toSet
		macro.contexts.forEach [ context |
			context.checkAllVariableReferencesAreKnownParameters(templateParameterNames,
				"Dereferenced variable must be a template variable of the macro itself")
		]
	}

	/**
	 *  check that each variable references used is known as parameterName(s)
	 */
	def void checkAllVariableReferencesAreKnownParameters(TestStepContext context, Set<String> parameterNames,
		String errorMessage) {
		switch context {
			ComponentTestStepContext:
				context.steps.forEach [
					checkAllVariableReferencesAreKnownParameters(parameterNames, errorMessage)
				]
			MacroTestStepContext:
				context.step.checkAllVariableReferencesAreKnownParameters(parameterNames, errorMessage)
			default:
				throw new RuntimeException('''Unknown TestStepContextType '«context.class.canonicalName»'.''')
		}
	}

	/**
	 * check that each deref variable used is known as parameterName(s)
	 */
	private def checkAllVariableReferencesAreKnownParameters(TestStep step, Set<String> parameterNames,
		String errorMessage) {
		val erroneousIndexedStepContents = step.contents.indexed.filterValue(StepContentVariableReference).filter [
			!parameterNames.contains(value.variable.name)
		]
		erroneousIndexedStepContents.forEach [
			error(errorMessage, value.eContainer, value.eContainingFeature, key, INVALID_VAR_DEREF)
		]
	}

	/** 
	 * get the actual jvm types from the fixtures that are transitively used and to which this variable/parameter is passed to
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
		String variableReference) {
		val stepsUsingThisVariable = componentTestStepContext.steps.filter [
			contents.filter(StepContentVariableReference).exists[variable.name == variableReference]
		]
		val typesUsages = stepsUsingThisVariable.map [ step |
			step.stepVariableFixtureParameterTypePairs.filterKey(StepContentVariableReference).filter [
				key.variable.name == variableReference
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
		val Map<String, String> varTypeMap = newHashMap
		macro.contexts.forEach[it.executeCheckVariableUsageWithinAssertionExpressions(varTypeMap)]
	}

	def dispatch void executeCheckVariableUsageWithinAssertionExpressions(
		ComponentTestStepContext componentTestStepContext, Map<String, String> varTypeMap) {
		executeTestStepCheckVariableUsageWithinAssertionExpressions(componentTestStepContext.steps, varTypeMap)
	}

	def dispatch void executeCheckVariableUsageWithinAssertionExpressions(MacroTestStepContext macroTestStepContext,
		Map<String, String> varTypeMap) {
		executeTestStepCheckVariableUsageWithinAssertionExpressions(#[macroTestStepContext.step], varTypeMap)
	}

	private def void executeTestStepCheckVariableUsageWithinAssertionExpressions(Iterable<TestStep> steps,
		Map<String, String> varTypeMap) {
		steps.forEach [ it, index |
			if (it instanceof TestStepWithAssignment) {
				// check "in order" (to prevent variable usage before assignment)
				if (varTypeMap.containsKey(variable.name)) {
					val message = '''Variable '«variable.name»' is assigned more than once.'''
					error(message, it, TmlPackage.Literals.TEST_STEP_WITH_ASSIGNMENT__VARIABLE, index,
						VARIABLE_ASSIGNED_MORE_THAN_ONCE);
				} else {
					varTypeMap.put(variable.name, interaction.defaultMethod.operation.returnType.identifier)
				}
			} else if (it instanceof AssertionTestStepImpl) {
				executeCheckVariableUsageWithinAssertionExpressions(varTypeMap, index)
			}
		]
	}

	private def executeCheckVariableUsageWithinAssertionExpressions(AssertionTestStep step,
		Map<String, String> varTypeMap, int index) {
		step.expression.collectVariableUsage.forEach [
			if (!varTypeMap.containsKey(variable.name)) { // regular variable dereference
				val message = '''Variable «if(variable.name!=null){ '\''+variable.name+'\''}» is unknown here.'''
				error(message, eContainer, eContainingFeature, VARIABLE_UNKNOWN_HERE)
			} else if (key != null) { // dereference map with a key
				val typeIdentifier = varTypeMap.get(variable.name).replaceFirst("<.*", "")
				if (typeIdentifier.
					isNotAssignableToMap) {
					val message = '''Variable '«variable.name»' of type '«typeIdentifier»' does not implement '«Map.canonicalName»'. It cannot be used with key '«key»'.'''
					error(message, eContainer, eContainingFeature, INVALID_MAP_REF)
				}
			}
		]
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

}
