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
package org.testeditor.rcp4.views.teststepselector

import org.eclipse.e4.core.di.annotations.Creatable
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.InteractionType
import org.testeditor.aml.TemplateText
import org.testeditor.aml.TemplateVariable

// TODO we should reuse AmlLabelProvider here where possible
/** provide labels that are used within the tree view for drag and drop into tcl documents */
@Creatable
class AmlModelLabelProvider {

	def dispatch String getText(AmlModel element) {
		element.package
	}

	def dispatch String getText(Component element) {
		element.name
	}

	def dispatch String getText(InteractionType element) {
		if( element.template == null ){
			"// template not defined yet"
		}else {
			element.template.contents.map[text].join(" ")		
		}
	}

	def dispatch String getText(TemplateText element) {
		element.value
	}

	def dispatch String getText(TemplateVariable element) {
		// TODO: selection whether variable or component element is referenced should not be based on the reference name!
		if (element.name == "element") {
			'''<«element.name»>''' // reference to a component element 
		} else {
			'''"«element.name»"''' // regular parameter
		}
	}

	def dispatch String getText(ComponentElement element) {
		element.name
	}

	// default
	def dispatch String getText(Object element) {
		element.toString
	}

}
