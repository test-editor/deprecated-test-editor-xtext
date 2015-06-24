package org.testeditor.aml.dsl.generator

import org.testeditor.aml.model.AmlModel
import org.testeditor.aml.model.InteractionType
import org.testeditor.aml.model.Template
import org.testeditor.aml.model.TemplateConstant
import org.testeditor.aml.model.TemplateContent
import org.testeditor.aml.model.TemplateVariable

class TechnicalBindingsGenerator {

	def CharSequence generateTechnicalBindings(AmlModel model) '''
		<?xml version="1.0" encoding="UTF-8"?>
		<TechnicalBindingTypes xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://testeditor.org/xsd_schema/v_1_1/TechnicalBindingTypeCollection.xsd" schemaVersion="1.1">
			«FOR interactionType : model.interactionTypes»
				«interactionType.generateTechnicalBinding»
			«ENDFOR»
		</TechnicalBindingTypes>
	'''

	protected def generateTechnicalBinding(InteractionType it) '''
		<TechnicalBindingType id="«name»" name="«label»">
			«template.generateActionParts»
		</TechnicalBindingType>
	'''

	protected def generateActionParts(Template template) '''
		«var i = 1»
		«FOR content : template.contents»
			«content.generateActionPart(i++)»
		«ENDFOR»
	'''
	
	protected dispatch def generateActionPart(TemplateConstant constant, int position) '''
		<actionPart position="«position»" type="TEXT" value="«constant.value»" />
	'''
	
	protected dispatch def generateActionPart(TemplateVariable variable, int position) {
		if (variable.name == "element") {
			return '''<actionPart position="«position»" type="ACTION_NAME" />'''
		} else {
			return '''<actionPart position="«position»" type="ARGUMENT" id="«variable.name»" />'''
		}
	}

	protected def String getActionPartType(TemplateContent content) {
		switch content {
			TemplateConstant: "TEXT"
			TemplateVariable case content.name == "element": "ACTION_NAME"
			TemplateVariable: "ARGUMENT"
		}
	}

}