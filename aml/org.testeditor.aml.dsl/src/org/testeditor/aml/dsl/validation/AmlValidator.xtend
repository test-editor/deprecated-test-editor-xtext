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
import java.util.Set
import java.util.regex.Pattern
import java.util.regex.PatternSyntaxException
import javax.inject.Inject
import org.eclipse.xtext.validation.Check
import org.eclipse.xtext.xtype.XImportSection
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.MethodReference
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.RegExValueSpace
import org.testeditor.aml.ValueSpaceAssignmentContainer
import org.testeditor.aml.Variable

import static org.testeditor.aml.AmlPackage.Literals.*
import static org.testeditor.aml.dsl.Messages.*
import static org.testeditor.dsl.common.CommonPackage.Literals.*

class AmlValidator extends AbstractAmlValidator {

	public static val COMPONENT__PARENTS__CYCLE = "component.parents.cycle"
	public static val COMPONENT__TYPE__MISSING = 'component.type.missing'
	public static val VARIABLE_REFERENCE_NAME__MISSING = 'variableReference.name.missing'
	public static val VALUE_SPACE_ASSIGNMENT__VARIABLE__NON_UNIQUE = 'valueSpaceAssignment.variable.nonUnique'
	public static val REG_EX_VALUE_SPACE__EXPRESSION__INVALID = "RegExValueSpace.expression.invalid"
	public static val COMPONENT_ELEMENT__LOCATOR_STRATEGY__MISSING = "componentElement.locatorStrategy.missing"
	public static val INTERACTION_NAME_DUPLICATION = "interactionType.name.duplication"

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
	def void checkVariableReferenceHasName(Variable variable) {
		if (variable.name.nullOrEmpty) {
			warning(
				Validation_VariableReference_MissingName,
				NAMED_ELEMENT__NAME,
				VARIABLE_REFERENCE_NAME__MISSING
			)
		}
	}

	/**
	 * Checks that value spaces are not assigned twice within an element.
	 */
	@Check
	def void checkValueSpaceAssignmentUnique(ValueSpaceAssignmentContainer container) {
		val variableToAssignment = container.valueSpaceAssignments.groupBy[variable]
		// those entries in the map with more than one value are duplicates
		variableToAssignment.entrySet.filter[value.size > 1].forEach[
			// TODO maybe improve position of error marker
			error(
				Validation_ValueSpaceAssignment_NonUnique,
				VALUE_SPACE_ASSIGNMENT__VARIABLE,
				VALUE_SPACE_ASSIGNMENT__VARIABLE__NON_UNIQUE
			)
		]
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
			defaultMethod !== null && !defaultMethod.locatorStrategyParameters.empty && locatorStrategy == null
		]
		if (elementHasNoStrategy && !interactionsExpectingButWithoutStrategy.empty) {
			val message = '''Element has interactions ('«interactionsExpectingButWithoutStrategy.map[name].join(', ')»') that require a locator strategy, but none is given.'''
			error(message, COMPONENT_ELEMENT__LOCATOR_STRATEGY, COMPONENT_ELEMENT__LOCATOR_STRATEGY__MISSING)
		}
	}

	override checkImports(XImportSection importSection) {
		// ignore for now
	}

	/** get a set of strings that occur more than once in the list passed */
	private def Set<String> getStringsOccuringMoreThanOnce(Iterable<String> list) {
		val stringsUnusedYet = list.toSet
		val stringsOccuringMoreThanOnce = list.dropWhile [
			val stringWasUnusedUpToNow = stringsUnusedYet.contains(it)
			if (stringWasUnusedUpToNow) {
				stringsUnusedYet.remove(it) // now used, remove from unused => if used a second time it is not dropped
			}
			return stringWasUnusedUpToNow
		]
		return stringsOccuringMoreThanOnce.toSet
	}

	@Check
	def void checkInteractionNameIsUnique(AmlModel amlModel) {
		amlModel.interactionTypes => [
			val doubleUsedInteractionNames = map[name].getStringsOccuringMoreThanOnce
			indexed.filter[doubleUsedInteractionNames.contains(value.name)].forEach [
				val message = '''Interaction has name ('«value.name»') which is used at least twice.'''
				error(message, value.eContainer, value.eContainingFeature, key, INTERACTION_NAME_DUPLICATION)
			]
		]
	}

}
		