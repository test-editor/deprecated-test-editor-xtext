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
package org.testeditor.aml

import java.util.Set
import org.eclipse.xtext.common.types.JvmType
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.common.types.TypesPackage

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
		val result = newHashSet
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
	 * @return all interaction types of the {@link Component} as well as its elements.
	 */
	def Set<InteractionType> getAllInteractionTypes(Component component) {
		return (component.componentInteractionTypes + component.componentElementsInteractionTypes).toSet
	}

	/**
	 * @return all interaction types of the {@link Component}.
	 */
	def Set<InteractionType> getComponentInteractionTypes(Component component) {
		return component.types.map[interactionTypes].flatten.toSet
	}

	/**
	 * @return all component elements of the {@link Component} including the elements through component inclusion.
	 */
	def Set<ComponentElement> getComponentElements(Component component) {
		return (component.elements + component.parents.map[componentElements].flatten).toSet
	}

	/**
	 * @return all interaction types of the component's elements.
	 */
	def Set<InteractionType> getComponentElementsInteractionTypes(Component component) {
		val componentElements = component.componentElements
		return componentElements.map[componentElementInteractionTypes].flatten.toSet
	}

	/**
	 * @return all interaction types of a {@link ComponentElement}.
	 */
	def Set<InteractionType> getComponentElementInteractionTypes(ComponentElement element) {
		return element.type.interactionTypes.toSet
	}

	/**
	 * @return all {@link TemplateVariable variables} that can be referenced
	 * 	from the outside, i.e. have a name that is not "element"
	 */
	def Set<TemplateVariable> getReferenceableVariables(Template template) {
		return template.contents.filter(TemplateVariable).filter [
			!name.nullOrEmpty && name != TEMPLATE_VARIABLE_ELEMENT
		].toSet
	}

	/**
	 * @return the fixture type if the given interaction type (may be null!)
	 */
	def JvmType getFixtureType(InteractionType interactionType) {
		return interactionType?.defaultMethod?.typeReference?.type
	}

	/**
	 * @return the type of the parameter of the fixture of the given interaction at position index (if present)
	 */
	def JvmTypeReference getTypeOfFixtureParameter(InteractionType interaction, int index) {
		val jvmParameters = interaction.defaultMethod.operation.parameters
		if (jvmParameters.size > index) {
			return jvmParameters.get(index).parameterType
		} else {
			return null
		}
	}

	/**
	 * @return the type returned by the fixture of the given interaction (may be null)
	 */
	def JvmTypeReference getReturnType(InteractionType interaction) {
		interaction.defaultMethod?.operation?.returnType
	}

	/**
	 * @return whether the given jvmTypeReference is assignable to clazz (without widening primitive types)
	 */
	def boolean isAssignableWithoutWidening(Class<?> clazz, JvmTypeReference jvmTypeReference) {
		val jvmTypeEClass = jvmTypeReference.type.eClass
		if (jvmTypeEClass == null) {
			return false
		} else {
			if (jvmTypeEClass != TypesPackage.Literals.JVM_PRIMITIVE_TYPE) {
				return (clazz.isAssignableFrom(Class.forName(jvmTypeReference.qualifiedName)))
			} else { // primitive types need to have identical name (no widening)
				return jvmTypeReference.qualifiedName == clazz.canonicalName
			}
		}
	}

}
