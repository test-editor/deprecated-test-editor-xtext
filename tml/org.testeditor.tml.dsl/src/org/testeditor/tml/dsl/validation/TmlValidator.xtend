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
import javax.inject.Inject
import org.eclipse.xtext.validation.Check
import org.eclipse.xtext.xtype.XImportSection
import org.testeditor.tml.AEVariableReference
import org.testeditor.tml.AssertionExpression
import org.testeditor.tml.AssertionTestStep
import org.testeditor.tml.BinaryAssertionExpression
import org.testeditor.tml.ComponentTestStepContext
import org.testeditor.tml.StepContentElement
import org.testeditor.tml.TestStep
import org.testeditor.tml.TestStepWithAssignment
import org.testeditor.tml.TmlPackage
import org.testeditor.tml.impl.AssertionTestStepImpl
import org.testeditor.tml.util.TmlModelUtil
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

	@Inject extension TmlModelUtil

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
