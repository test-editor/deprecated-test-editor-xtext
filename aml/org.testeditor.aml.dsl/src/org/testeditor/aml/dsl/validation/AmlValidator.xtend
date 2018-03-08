/*******************************************************************************
 * Copyright (c) 2012 - 2018 Signal Iduna Corporation and others.
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
import java.util.Map
import java.util.regex.Pattern
import java.util.regex.PatternSyntaxException
import javax.inject.Inject
import org.eclipse.xtext.validation.Check
import org.eclipse.xtext.xtype.XImportSection
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.ElementTypeWithInteractions
import org.testeditor.aml.InteractionType
import org.testeditor.aml.MethodReference
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.RegExValueSpace
import org.testeditor.aml.Template
import org.testeditor.aml.TemplateText
import org.testeditor.aml.ValueSpaceAssignmentContainer
import org.testeditor.aml.Variable
import org.testeditor.dsl.common.util.CollectionUtils

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
	public static val TEMPLATE_CODE_NOT_UNIQUE = "templateCode.not.unique"
	public static val INVALID_CHAR_IN_TEMPLATE = "template.string.invalidChar"

	@Inject
	private extension ModelUtil
	
	@Inject
	private extension CollectionUtils

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
		variableToAssignment.entrySet.filter[value.size > 1].forEach [
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

	@Check
	def void checkInteractionNameIsUnique(AmlModel amlModel) {
		val mapInteractionName2InteractionTypes = newHashMap

		amlModel.interactionTypes.forEach [
			if (mapInteractionName2InteractionTypes.containsKey(it.name)) {
				val message = MessageFormat.format(Validation_InteractionType_Name_Dublicate, it.name)
				error(message, mapInteractionName2InteractionTypes.get(it.name),
					mapInteractionName2InteractionTypes.get(it.name).eContainer.eContainingFeature, INTERACTION_NAME_DUPLICATION)
				error(message, it, it.eContainer.eContainingFeature, INTERACTION_NAME_DUPLICATION)
			} else {
				mapInteractionName2InteractionTypes.put(it.name, it)
			}
		]
	}

	@Check
	def void checkDupplicateTemplatesInComponentType(ElementTypeWithInteractions componentType) {
		val mapTemplateCode2InteractionType = newHashMap

		componentType.interactionTypes.forEach [
			var templateCode = template.normalize 

			if (mapTemplateCode2InteractionType.containsKey(templateCode)) {
				error(Validation_TemplateCode_NotUnique, componentType,
						componentType.eContainer.eContainingFeature, TEMPLATE_CODE_NOT_UNIQUE)
				error(Validation_TemplateCode_NotUnique, mapTemplateCode2InteractionType.get(templateCode),
					mapTemplateCode2InteractionType.get(templateCode).eContainer.eContainingFeature,
					TEMPLATE_CODE_NOT_UNIQUE)
				error(Validation_TemplateCode_NotUnique, it, it.eContainer.eContainingFeature,
						TEMPLATE_CODE_NOT_UNIQUE)
			} else {
				mapTemplateCode2InteractionType.put(templateCode, it)
			}
		]
	}
	
	static val ID_REGEX='\\^?[a-zA-Z$_À-ɿΑ-ѯµ][a-zA-Z$_0-9À-ɿΑ-ѯµ]*'
	static val VALID_TEMPLATE_WORDS = '''^[ \t]*(«ID_REGEX»[ \t]*)+$'''
	static val VALID_LAST_TEMPLATE_WORDS = '''^[ \t]*(«ID_REGEX»[ \t]*)*([.?][ \t]*)?$'''
	
	/**
	 * This validation is tightly coupled with the parser rules for tcl-template usages (see Tcl.xtext:TestStep and Tcl.xtext:TestStepAssignment)
	 */
	@Check
	def void checkTemplateHoldsValidCharacters(Template template) {
		template.contents => [
			// all text elements must not be empty (checked here to allow index-based error marking)
			val templateTextsThatAreEmpty = indexed.filterValue(TemplateText).filter[value.value.trim.empty]
			templateTextsThatAreEmpty.forEach [
				error('Template string must not be empty', value.eContainer, value.eContainingFeature, key,
					INVALID_CHAR_IN_TEMPLATE)
			]
			// all but the last text element must match the VALID_TEMPLATE_WORD
			val invalidTemplateTextsWithoutLastElement = indexed.butLast.filterValue(TemplateText).filter [
				!value.value.matches(VALID_TEMPLATE_WORDS)
			]
			invalidTemplateTextsWithoutLastElement.forEach [
				error('Illegal characters in template. Please use ids only (e.g. do not make use of punctuation and the like)',
					value.eContainer, value.eContainingFeature, key, INVALID_CHAR_IN_TEMPLATE)
			]
			// (only) the last template content (if it is a text) may end with a punctuation mark '.' | '?'
			val lastContent = it.last
			val lastIndex = size - 1
			if (lastContent instanceof TemplateText) {
				if (!lastContent.value.matches(VALID_LAST_TEMPLATE_WORDS)) {
					error('Illegal characters in last element of template. Please use ids only (e.g. use punctuation like . or ? only at the very end)',
						lastContent.eContainer, lastContent.eContainingFeature, lastIndex, INVALID_CHAR_IN_TEMPLATE)
				}
			}
		]
	}
}
			
