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
import org.testeditor.aml.TemplateVariable
import org.testeditor.tml.AEVariableReference
import org.testeditor.tml.AssertionExpression
import org.testeditor.tml.AssertionTestStep
import org.testeditor.tml.BinaryAssertionExpression
import org.testeditor.tml.ComponentTestStepContext
import org.testeditor.tml.Macro
import org.testeditor.tml.MacroTestStepContext
import org.testeditor.tml.StepContentElement
import org.testeditor.tml.TestStep
import org.testeditor.tml.TestStepContext
import org.testeditor.tml.TestStepWithAssignment
import org.testeditor.tml.TmlPackage
import org.testeditor.tml.impl.AssertionTestStepImpl
import org.testeditor.tml.util.TmlModelUtil
import org.testeditor.tsl.StepContent
import org.testeditor.tsl.StepContentVariable
import org.testeditor.tsl.TslPackage
import org.testeditor.aml.ModelUtil
import org.testeditor.tml.StepContentVariableReference

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
	@Inject extension ModelUtil

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
			val macroModel = testStep.macroContext.macroModel
			if (!macroModel.macros.exists[template.normalize == normalizedTeststep]) {
				warning("test step could not resolve macro usage", TmlPackage.Literals.TEST_STEP__CONTENTS,
					MISSING_MACRO)
			}
		}
	}

	@Check
	def checkMacroParameterUsage(Macro macro) {
		val templateParameterNames = macro.template.contents.filter(TemplateVariable).map[name].toSet
		macro.contexts.forEach [ context |
			context.checkAllDerefVariableAreKnownParmeters(templateParameterNames,
				"Dereferenced variable must be a template variable of the macro itself")
		]
	}

	/**
	 *  check that each deref variable used is known as parameterName(s)
	 */
	def void checkAllDerefVariableAreKnownParmeters(TestStepContext context, Set<String> parameterNames, String errorMessage) {
		switch context {
			ComponentTestStepContext: context.steps.forEach[checkAllDerefVariableAreKnownParmeters(parameterNames, errorMessage)]
			MacroTestStepContext: context.step.checkAllDerefVariableAreKnownParmeters(parameterNames, errorMessage)
			default: throw new RuntimeException('''Unknown TestStepContextType '«context.class.canonicalName»'.''')
		}
	}

	/**
	 * check that each deref variable used is known as parameterName(s)
	 */
	private def checkAllDerefVariableAreKnownParmeters(TestStep step, Set<String> parameterNames, String errorMessage) {
		step.contents.forEach [ it, idx |
			if (it instanceof StepContentVariableReference && !parameterNames.contains(value)) {
				error(errorMessage, eContainer, eContainingFeature, idx, INVALID_VAR_DEREF)
			}
		]
	}

	/** 
	 * get the actual jvm types from the fixtures that are transitively used and to which this variable/parameter is passed to
	 */
	def dispatch Set<JvmTypeReference> getTypeUsagesOfVariable(MacroTestStepContext macroTestStepContext,
		String variable) {
		val macro = macroTestStepContext.findMacroDefinition
		if (macro != null) {
			val varMap = getVariableToValueMapping(macroTestStepContext.step, macro.template)
			val parametersThatGetVariablePassedIn = varMap.filter[key, stepContent|stepContent instanceof StepContentVariableReference && (stepContent as StepContentVariableReference).variable.name == variable].
				keySet.map[name].toSet
			val relevantContexts = macro.contexts.filter [
				makesUseOfVariablesViaDeref(parametersThatGetVariablePassedIn)
			]
			val typesOfAllParametersUsed = relevantContexts.map [ context |
				parametersThatGetVariablePassedIn.map [ parameter |
					context.getTypeUsagesOfVariable(parameter)
				].flatten
			].flatten.toSet
			return typesOfAllParametersUsed
		} else {
			return #{}
		}
	}

	/**
	 * does the given context make use of (one of the) variables passed?
	 */
	private def boolean makesUseOfVariablesViaDeref(TestStepContext context, Set<String> variables) {
		switch context {
			ComponentTestStepContext: context.steps.exists[contents.exists[makesUseOfVariablesViaDeref(variables)]]
			MacroTestStepContext: context.step.contents.exists[makesUseOfVariablesViaDeref(variables)]
			default: false
		}
	}

	/**
	 * does the given step make use of (one of the) variables passed?
	 */
	private def boolean makesUseOfVariablesViaDeref(StepContent stepContent, Set<String> variables) {
		return stepContent instanceof StepContentVariableReference && variables.contains((stepContent as StepContentVariableReference).variable.name)
	}

	/** 
	 * get the actual jvm types from the fixtures that are transitively used and to which this variable/parameter is passed to
	 */
	def dispatch Set<JvmTypeReference> getTypeUsagesOfVariable(ComponentTestStepContext componentTestStepContext,
		String variable) {
		val releveantSteps = componentTestStepContext.steps.filter [
			contents.exists[it instanceof StepContentVariableReference && (it as StepContentVariableReference).variable.name == variable]
		]
		val typesOfAllParametersUsed = releveantSteps.map [ step |
			val parameters = step.contents.filter [
				it instanceof StepContentVariable || it instanceof StepContentVariableReference ||
					it instanceof StepContentElement
			]
			val indicesOfParametersThatGetVariablePassedIn = parameters.indexed.filter [
				value instanceof StepContentVariableReference && (value as StepContentVariableReference).variable.name == variable
			].map[key]
			return indicesOfParametersThatGetVariablePassedIn.map [ index |
				step.interaction?.getTypeOfFixtureParameter(index)
			].filterNull
		].flatten.toSet
		return typesOfAllParametersUsed
	}

	@Check
	def checkVariableUsageWithinAssertionExpressions(ComponentTestStepContext tsContext) {
		val varTypeMap = newHashMap
		// collect all var assignments
		tsContext.steps.forEach [ it, index |
			if (it instanceof TestStepWithAssignment) {
				// check "in order" (to prevent variable usage before assignment)
				if (varTypeMap.containsKey(variableName)) {
					val message = '''Variable '«variableName»' is assigned more than once.'''
					warning(message, TmlPackage.Literals.COMPONENT_TEST_STEP_CONTEXT__STEPS, index,
						VARIABLE_ASSIGNED_MORE_THAN_ONCE);
				} else {
					varTypeMap.put(variableName, interaction.defaultMethod.operation.returnType.identifier)
				}
			} else if (it instanceof AssertionTestStepImpl) {
				executeCheckVariableUsageWithinAssertionExpressions(varTypeMap, index)
			}
		]
	}

	private def executeCheckVariableUsageWithinAssertionExpressions(AssertionTestStep step,
		Map<String, String> varTypeMap, int index) {
		step.expression.collectVariableUsage.forEach [
			if (varTypeMap.get(name) == null) { // regular variable dereference
				val message = '''Variable '«name»' is unknown here.'''
				error(message, eContainer, eContainingFeature, VARIABLE_UNKNOWN_HERE)
			} else if (key != null) { // dereference map with a key
				val typeIdentifier = varTypeMap.get(name).replaceFirst("<.*", "")
				if (typeIdentifier.isNotAssignableToMap) {
					val message = '''Variable '«name»' of type '«typeIdentifier»' does not implement '«Map.canonicalName»'. It cannot be used with key '«key»'.'''
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
		var valueSpace = stepContentVariable.valueSpaceAssignment.valueSpace
		if (!valueSpace.isValidValue(stepContentVariable.value)) {
			val message = '''Value is not allowed in this step. Allowed values: '«valueSpace»'.'''
			warning(message, TslPackage.Literals.STEP_CONTENT__VALUE, UNALLOWED_VALUE);
		}
	}

}
