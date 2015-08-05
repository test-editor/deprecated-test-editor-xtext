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
package org.testeditor.aml.model

import java.util.HashSet
import java.util.Set

class ModelUtil {

	// TODO implement hasParentsCycle and test!
	public static final String TEMPLATE_VARIABLE_ELEMENT = "element"

	/**
	 * Checks whether the {@link Component}'s parent hierarchy
	 * contains a cycle or not.
	 */
	def boolean hasParentsCycle(Component component) {
		return false
	}

	def Set<ComponentType> getTypes(Component component) {
		val result = new HashSet
		component.collectTypes(result)
		return result
	}

	private def void collectTypes(Component component, Set<ComponentType> result) {
		if (component.type !== null) {
			result += component.type
		}
		component.parents.forEach[collectTypes(result)]
	}

	/**
	 * @return all {@link TemplateVariable variables} that can be referenced
	 * 	from the outside, i.e. have a name that is not "element"
	 */
	def Set<TemplateVariable> getReferenceableVariables(Template template) {
		return template.contents.filter(TemplateVariable).filter[
			!name.nullOrEmpty && name != TEMPLATE_VARIABLE_ELEMENT
		].toSet
	}

}