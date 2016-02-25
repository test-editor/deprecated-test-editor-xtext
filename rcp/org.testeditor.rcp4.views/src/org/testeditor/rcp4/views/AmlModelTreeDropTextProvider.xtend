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
package org.testeditor.rcp4.views

import org.eclipse.e4.core.di.annotations.Creatable
import org.eclipse.emf.ecore.EObject
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.InteractionType

/** provide the text actually inserted after a drag and drop action.
 *  the text is adjusted by all elements on the selected (dragged) tree path. 
 *  distance 0.. is the number of hops to the actually dragged element (0 = element itself is dragged).
 * 
 *  this enables adjustments (e.g. replacements) of certain strings given the context
 *  of the whole path. e.g. replace "element" with the actual component element name.
 */
@Creatable
class AmlModelTreeDropTextProvider {
	def dispatch String adjustTextBy(int distance, ComponentElement element, String basis) {
		basis.replace("element", element.name)
	}

	def dispatch String adjustTextBy(int distance, InteractionType element, String basis) {
		(if(distance == 0) '- ' else '') + basis + (if(distance == 0) '\n' else '')
	}

	def dispatch String adjustTextBy(int distance, Component element, String basis) {
		(if(distance == 0) 'Mask: ' else '') + basis + (if(distance == 0) '\n' else '')
	}

	/** default, no adjustment */
	def dispatch String adjustTextBy(int distance, Object element, String basis) {
		basis
	}
}
