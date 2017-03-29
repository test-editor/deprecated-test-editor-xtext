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
package org.testeditor.aml.dsl.scoping

import com.google.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EReference
import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.eclipse.xtext.scoping.IScope
import org.eclipse.xtext.scoping.Scopes
import org.eclipse.xtext.xbase.scoping.batch.XbaseBatchScopeProvider
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.ElementTypeWithInteractions
import org.testeditor.aml.ElementWithInteractions
import org.testeditor.aml.InteractionType
import org.testeditor.aml.MethodReference
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.TemplateVariable
import org.testeditor.aml.ValueSpaceAssignment

import static org.testeditor.aml.AmlPackage.Literals.*

class AmlScopeProvider extends XbaseBatchScopeProvider {

	@Inject extension ModelUtil
	@Inject extension IQualifiedNameConverter

	@Inject MethodReferenceScopes methodReferenceScopes
	@Inject LocatorStrategyScopes locatorStrategyScopes

	override getScope(EObject context, EReference reference) {
		if (reference == VALUE_SPACE_ASSIGNMENT__VARIABLE) {
			if (context instanceof ElementWithInteractions<?>) {
				// TODO do we ever enter this if-block
				return context.type.interactionsTemplateVariablesScope
			}
			if (context instanceof ValueSpaceAssignment) {
				return context.valueSpaceAssignmentVariableScope
			}
		}

		if (context instanceof MethodReference) {
			if (reference == METHOD_REFERENCE__OPERATION) {
				return methodReferenceScopes.scopeFor_MethodReference_operation(context)
			} else if (reference == METHOD_REFERENCE__PARAMETERS) {
				return methodReferenceScopes.scopeFor_MethodReference_parameters(context)
			}
		}

		if (context instanceof InteractionType) {
			if (reference == INTERACTION_TYPE__LOCATOR_STRATEGY) {
				return locatorStrategyScopes.getLocatorStrategyScope(context)
			}
		}

		if (context instanceof ComponentElement) {
			if (reference == COMPONENT_ELEMENT__LOCATOR_STRATEGY) {
				return locatorStrategyScopes.getLocatorStrategyScope(context)
			}
		}

		return super.getScope(context, reference)
	}

	/**
	 * Provides the scoping for template variables of visible interactions.
	 */
	private def IScope getInteractionsTemplateVariablesScope(ElementTypeWithInteractions type) {
		if (type === null) {
			return IScope.NULLSCOPE
		}
		val variables = type.templateVariablesInScope
		// Calculate a "partially qualified name" here to reference as InteractionType.variable
		return Scopes.scopeFor(variables, [ variable |
			val interactionType = variable.eContainer?.eContainer
			if (interactionType instanceof InteractionType) {
				return '''«interactionType.name».«variable.name»'''.toString.toQualifiedName
			}
			return null
		], IScope.NULLSCOPE)
	}

	/**
	 * Provides scoping for referenceable variables for a {@link ValueSpaceAssignment}.
	 */
	private def IScope getValueSpaceAssignmentVariableScope(ValueSpaceAssignment context) {
		val container = context.eContainer
		if (container instanceof ElementWithInteractions<?>) {
			return container.type.interactionsTemplateVariablesScope
		} else if (container instanceof ElementTypeWithInteractions) {
			return container.interactionsTemplateVariablesScope
		} else if (container instanceof InteractionType) {
			val referenceableVariables = container.template.referenceableVariables
			return Scopes.scopeFor(referenceableVariables, IScope.NULLSCOPE)
		}
		return IScope.NULLSCOPE
	}

	/**
	 * @return the {@link TemplateVariable variables} that can be referenced from the passed type
	 */
	private def Iterable<TemplateVariable> getTemplateVariablesInScope(ElementTypeWithInteractions type) {
		val templates = type.interactionTypes.map[template].filterNull
		val variables = templates.map[referenceableVariables].flatten
		return variables
	}

}
