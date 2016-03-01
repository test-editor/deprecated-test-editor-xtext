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
package org.testeditor.aml.legacy.importer

import java.util.List
import javax.inject.Singleton
import org.testeditor.aml.InteractionType
import org.testeditor.aml.AmlFactory
import org.testeditor.aml.TemplateContent
import org.testeditor.xmllibrary.domain.binding.ActionPart
import org.testeditor.xmllibrary.domain.binding.TechnicalBindingType
import org.testeditor.xmllibrary.domain.binding.TechnicalBindingTypes

/**
 * Converts {@link TechnicalBindingType} into {@link InteractionType}.
 */
@Singleton
class TechnicalBindingConverter {
	
	static val factory = AmlFactory.eINSTANCE
	static val ACTION_NAME_VARIABLE_NAME = "element" // TODO inject with named annotation
	
	def List<InteractionType> convert(TechnicalBindingTypes technicalBindings) {
		return technicalBindings.technicalBindingType.map[convert].toList	
	}
	
	def InteractionType convert(TechnicalBindingType technicalBinding) {
		val result = factory.createInteractionType
		result => [
			name = technicalBinding.id.escapeId
			label = technicalBinding.name
			template = factory.createTemplate
			template.contents += technicalBinding.actionPart.map[convert]
		]
		return result
	}
	
	protected def TemplateContent convert(ActionPart actionPart) {
		return switch actionPart.type {
			case TEXT: factory.createTemplateText => [
				value = actionPart.value
			]
			case ACTION_NAME: factory.createTemplateVariable => [
				name = ACTION_NAME_VARIABLE_NAME
			]
			case ARGUMENT: factory.createTemplateVariable => [
				name = actionPart.id.escapeId
			]
		}
	}
	
	protected def escapeId(String id) {
		if (id.nullOrEmpty) {
			return ""
		}
		return id.replaceAll("ö", "oe").replaceAll("ä", "ae").replaceAll("ü", "ue")
	}
	
}