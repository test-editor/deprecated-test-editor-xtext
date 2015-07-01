package org.testeditor.aml.dsl.generator

import org.testeditor.aml.model.AmlModel
import org.testeditor.aml.model.Component
import org.testeditor.aml.model.ComponentElement
import org.testeditor.aml.model.InteractionType

class AllActionGroupsGenerator extends AbstractGenerator {
	
	def CharSequence generateAllActionGroups(AmlModel model) '''
		<?xml version="1.0" encoding="UTF-8"?>
		<ActionGroups xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://testeditor.org/xsd_schema/v_1_1/AllActionGroups.xsd" schemaVersion="1.1">
			«FOR component : model.components»
				«component.generateActionGroup»
			«ENDFOR»
		</ActionGroups>
	'''
	
	protected def generateActionGroup(Component component) '''
		<ActionGroup name="«component.labelOrName»" id="«component.name»">
			«FOR element : component.elements»
				«element.generateActions»
			«ENDFOR»
		</ActionGroup>
	'''
	
	protected def generateActions(ComponentElement element) '''
		«FOR interaction : element.type.interactionTypes»
			«element.generateAction(interaction)»
		«ENDFOR»
	'''
	
	protected def generateAction(ComponentElement element, InteractionType interaction) '''
		<action technicalBindingType="«interaction.name»">
			«IF !element.locator.nullOrEmpty»
				<actionName locator="«element.locator»">«element.name»</actionName>
			«ENDIF»
«««			TODO handle arguments
		</action>
	'''
	
}