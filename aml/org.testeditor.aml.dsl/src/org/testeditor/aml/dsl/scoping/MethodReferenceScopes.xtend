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
package org.testeditor.aml.dsl.scoping

import javax.inject.Inject
import org.eclipse.xtext.common.types.JvmDeclaredType
import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference
import org.eclipse.xtext.common.types.JvmVisibility
import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.eclipse.xtext.scoping.IScope
import org.eclipse.xtext.scoping.Scopes
import org.testeditor.aml.InteractionType
import org.testeditor.aml.MethodReference
import org.testeditor.aml.TemplateVariable

import static extension org.eclipse.xtext.EcoreUtil2.getContainerOfType

/**
 * Provides scoping for {@link MethodReference}.
 */
class MethodReferenceScopes {
	
	@Inject extension IQualifiedNameConverter 
	
	/**
	 * Provides scoping for MethodReference.operation.
	 */
	def IScope scopeFor_MethodReference_operation(MethodReference context) {
		val fixtureMethods = getFixtureMethods(context.typeReference)
		return Scopes.scopeFor(fixtureMethods, [simpleName.toQualifiedName], IScope.NULLSCOPE)
	}

	/**
	 * Provides scoping for MethodReference.parameters.
	 */
	def IScope scopeFor_MethodReference_parameters(MethodReference context) {
		val interactionType = context.getContainerOfType(InteractionType)
		val availableVariables = interactionType.template.contents.filter(TemplateVariable)
		return Scopes.scopeFor(availableVariables)
	}

	/**
	 * @param typeReference may be {@code null}
	 * @return all public, non-static operations that are annotated with @FixtureMethod, inherited methods included, never {@code null}
	 */
	private def Iterable<JvmOperation> getFixtureMethods(JvmParameterizedTypeReference typeReference) {
		val type = typeReference?.type
		if (type instanceof JvmDeclaredType) {
			return getFixtureMethods(type)
		}
		return emptyList
	}

	private def Iterable<JvmOperation> getFixtureMethods(JvmDeclaredType type) {
		return type.allFeatures.filter(JvmOperation).filter[
			return (visibility == JvmVisibility.PUBLIC) && !isStatic && hasFixtureMethodAnnotation
		]
	}

	/**
	 * @return {@code true} if the operation is annotated with {@code org.testeditor.fixture.core.interaction.FixtureMethod}.
	 */
	private def boolean hasFixtureMethodAnnotation(JvmOperation operation) {
		// TODO do we want a hard-coded reference here or a dependency on core-fixture?
		return operation.annotations.exists[annotation.qualifiedName == "org.testeditor.fixture.core.interaction.FixtureMethod"]
	}
	
}