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
package org.testeditor.aml.dsl.generator

import com.google.common.annotations.VisibleForTesting
import org.testeditor.aml.AmlModel
import org.testeditor.aml.InteractionType
import org.testeditor.aml.Template
import org.testeditor.aml.TemplateText
import org.testeditor.aml.TemplateVariable

/**
 * Generates the legacy XML for TechnicalBindingTypes
 */
class TechnicalBindingsGenerator extends AbstractGenerator {

	public static val XML_HEADER = '''<?xml version="1.0" encoding="UTF-8"?>'''
	public static val OPEN_TAG = '''<TechnicalBindingTypes xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://testeditor.org/xsd_schema/v_1_1/TechnicalBindingTypeCollection.xsd" schemaVersion="1.1">'''
	public static val CLOSE_TAG = '''</TechnicalBindingTypes>'''

	def String generateTechnicalBindings(AmlModel model) '''
		«XML_HEADER»
		«OPEN_TAG»
			«FOR interactionType : model.interactionTypes»
				«interactionType.generateTechnicalBinding»
			«ENDFOR»
		«CLOSE_TAG»
	'''

	@VisibleForTesting
	def generateTechnicalBinding(InteractionType it) '''
		<TechnicalBindingType id="«name»" name="«labelOrName»">
			«template?.generateActionParts»
		</TechnicalBindingType>
	'''

	protected def generateActionParts(Template template) '''
		«var i = 1»
		«FOR content : template.contents»
			«content.generateActionPart(i++)»
		«ENDFOR»
	'''

	protected dispatch def generateActionPart(TemplateText constant, int position) '''
		<actionPart position="«position»" type="TEXT" value="«constant.value»" />
	'''

	protected dispatch def generateActionPart(TemplateVariable variable, int position) {
		if (variable.name == "element") {
			return '''<actionPart position="«position»" type="ACTION_NAME" />'''
		} else {
			return '''<actionPart position="«position»" type="ARGUMENT"«IF !variable.name.nullOrEmpty» id="«variable.name»"«ENDIF» />'''
		}
	}

}