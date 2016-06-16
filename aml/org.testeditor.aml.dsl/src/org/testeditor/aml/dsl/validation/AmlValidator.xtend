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
package org.testeditor.aml.dsl.validation

import java.text.MessageFormat
import java.util.regex.Pattern
import java.util.regex.PatternSyntaxException
import javax.inject.Inject
import org.eclipse.xtext.validation.Check
import org.eclipse.xtext.xtype.XImportSection
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.MethodReference
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.RegExValueSpace
import org.testeditor.aml.ValueSpaceAssignment
import org.testeditor.aml.VariableReference

import static org.testeditor.aml.AmlPackage.Literals.*
import static org.testeditor.aml.dsl.Messages.*

class AmlValidator extends AbstractAmlValidator {

	public static val COMPONENT__PARENTS__CYCLE = "component.parents.cycle"
	public static val COMPONENT__TYPE__MISSING = 'component.type.missing'
	public static val VARIABLE_REFERENCE_NAME__MISSING = 'variableReference.name.missing'
	public static val VALUE_SPACE_ASSIGNMENT__VARIABLE__NON_UNIQUE = 'valueSpaceAssignment.variable.nonUnique'
	public static val REG_EX_VALUE_SPACE__EXPRESSION__INVALID = "RegExValueSpace.expression.invalid"
	public static val COMPONENT_ELEMENT__LOCATOR_STRATEGY__MISSING = "componentElement.locatorStrategy.missing"

	@Inject
	private extension ModelUtil

	/**
	 * Checks that a {@link Component} does not have a cycle in its parents hierarchy.
	 */
	@Check
	def void checkComponentHasNoCycle(Component component) {
		if (component.hasParentsCycle) {
			error(
				Validation_Component_Cycle,
				COMPONENT__PARENTS,
				COMPONENT__PARENTS__CYCLE
			)
		}
	}

	/**
	 * Checks that a {@link Component} has a type (declared or inherited).
	 */
	@Check
	def void checkComponentHasType(Component component) {
		if (!component.hasParentsCycle && component.types.empty) {
			error(
				Validation_Component_Type_Missing,
				ELEMENT_WITH_INTERACTIONS__TYPE,
				COMPONENT__TYPE__MISSING
			)
		}
	}

	/**
	 * Checks if a template variable has a name, if not => warning
	 */
	@Check
	def void checkVariableReferenceHasName(VariableReference variable) {
		if (variable.name.nullOrEmpty) {
			warning(
				Validation_VariableReference_MissingName,
				VARIABLE_REFERENCE__NAME,
				VARIABLE_REFERENCE_NAME__MISSING
			)
		}
	}

	/**
	 * Checks that value spaces are not assigned twice within an element.
	 */
	@Check
	def void checkValueSpaceAssignmentUnique(ValueSpaceAssignment assignment) {
		val element = assignment.element
		val duplicate = element.valueSpaceAssignments.findFirst [
			it !== assignment && variable === assignment.variable
		]
		if (duplicate !== null) {
			error(
				Validation_ValueSpaceAssignment_NonUnique,
				VALUE_SPACE_ASSIGNMENT__VARIABLE,
				VALUE_SPACE_ASSIGNMENT__VARIABLE__NON_UNIQUE
			)
		}
	}

	/**
	 * Checks that the correct amount of parameters is referenced.
	 */
	@Check
	def void checkMethodReferenceParameters(MethodReference reference) {
		val operation = reference.operation
		if (operation !== null) {
			if (operation.parameters.size != reference.parameters.size + reference.locatorStrategyParameters.size) {
				error(
					Validation_MethodReference_InvalidParameterList,
					reference,
					METHOD_REFERENCE__OPERATION
				)
			}
		}
	}

	/** 
	 * Checks that the specified expression is a valid regular expression. 
	 */
	@Check
	def void checkRegExValueSpace(RegExValueSpace valueSpace) {
		try {
			Pattern.compile(valueSpace.expression)
		} catch (PatternSyntaxException e) {
			val message = MessageFormat.format(Validation_RegExValueSpace_InvalidRegEx, e)
			error(
				message,
				REG_EX_VALUE_SPACE__EXPRESSION,
				REG_EX_VALUE_SPACE__EXPRESSION__INVALID
			)
		}
	}

	/**
	 * Check that elements or interactions must provides locators if the fixture operation needs those
	 */
	@Check
	def void checkComponentElementLocatorStrategy(ComponentElement componentElement) {
		val elementHasNoStrategy = componentElement.locatorStrategy == null
		val interactionsExpectingButWithoutStrategy = componentElement.componentElementInteractionTypes.filter [
			!defaultMethod.locatorStrategyParameters.empty && locatorStrategy == null
		]
		if (elementHasNoStrategy && !interactionsExpectingButWithoutStrategy.empty) {
			val message = '''Element has interactions ('«interactionsExpectingButWithoutStrategy.map[name].join(', ')»') that require a locator strategy, but none is given.'''
			error(message, COMPONENT_ELEMENT__LOCATOR_STRATEGY, COMPONENT_ELEMENT__LOCATOR_STRATEGY__MISSING)
		}
	}

	override checkImports(XImportSection importSection) {
		// ignore for now
	}

}
