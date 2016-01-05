/*******************************************************************************
 * Copyright (c) 2012 - 2015 Signal Iduna Corporation and others.
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
package org.testeditor.aml.dsl.generator

import org.testeditor.aml.model.AmlModel
import org.testeditor.aml.model.Component
import org.testeditor.aml.model.ComponentElement
import org.testeditor.aml.model.ElementWithInteractions
import org.testeditor.aml.model.IntegerRange
import org.testeditor.aml.model.InteractionType
import org.testeditor.aml.model.StringLiterals
import org.testeditor.aml.model.TemplateVariable
import org.testeditor.aml.model.ValueSpaceAssignment

class AllActionGroupsGenerator extends AbstractGenerator {
	
	public static val XML_HEADER = '''<?xml version="1.0" encoding="UTF-8"?>'''
	public static val OPEN_TAG = '''<ActionGroups xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://testeditor.org/xsd_schema/v_1_1/AllActionGroups.xsd" schemaVersion="1.1">'''
	public static val CLOSE_TAG = '''</ActionGroups>'''
	
	def String generateAllActionGroups(AmlModel model) '''
		«XML_HEADER»
		«OPEN_TAG»
			«FOR component : model.components»
				«component.generateActionGroup»
			«ENDFOR»
		«CLOSE_TAG»
	'''
	
	protected def generateActionGroup(Component component) '''
		<ActionGroup name="«component.labelOrName»">
			«component.generateActions»
			«FOR element : component.elements»
				«element.generateActions»
			«ENDFOR»
		</ActionGroup>
	'''
	
	protected def generateActions(ElementWithInteractions<?> element) '''
		«IF element.type !== null»
			«FOR interaction : element.type.interactionTypes»
				«element.generateAction(interaction)»
			«ENDFOR»
		«ENDIF»
	'''

	protected def generateAction(ElementWithInteractions<?> element, InteractionType interaction) {
		val actionName = element.actionName
		val arguments = element.getArguments(interaction)
		if (actionName.nullOrEmpty && arguments.nullOrEmpty) {
			return '''<action technicalBindingType="«interaction.name»" />'''
		} else {
			return '''
				<action technicalBindingType="«interaction.name»">
					«actionName»
					«arguments»
				</action>
			'''
		}
	}
	
	protected def getActionName(ElementWithInteractions<?> element) {
		if (element instanceof ComponentElement) {
			return '''<actionName locator="«element.locator»">«element.labelOrName»</actionName>'''
		} // else: no actioName if the element is a component	
		return ""
	}
	
	protected def getArguments(ElementWithInteractions<?> element, InteractionType interaction) {
		if (interaction.template === null) {
			return "" // if there is no template, we don't have arguments to consider
		}
		val variablesInScope = interaction.template.contents.filter(TemplateVariable).toList
		val assignments = element.valueSpaceAssignments.filter[variablesInScope.contains(variable)]
		return '''
			«FOR assignment : assignments»
				«assignment.generateArgument»
			«ENDFOR»
		'''
	}
	
	protected def generateArgument(ValueSpaceAssignment assignment) '''
		<argument id="«assignment.variable.name»">
			«assignment.valueSpace.generateValues»
		</argument>
	'''
	
	protected def dispatch generateValues(StringLiterals stringLiterals) '''
		«FOR value : stringLiterals.values»
			<value>«value»</value>
		«ENDFOR»
	'''
	
	protected def dispatch generateValues(IntegerRange integerRange) '''
		«FOR value : (integerRange.from .. integerRange.to)»
			<value>«value»</value>
		«ENDFOR»
	'''
	
}