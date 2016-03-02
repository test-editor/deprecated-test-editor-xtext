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

import javax.inject.Inject
import org.eclipse.emf.ecore.EReference
import org.eclipse.xtext.common.types.JvmDeclaredType
import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.common.types.JvmVisibility
import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.eclipse.xtext.scoping.IScope
import org.eclipse.xtext.scoping.Scopes
import org.testeditor.aml.MethodReference

import static org.testeditor.aml.AmlPackage.Literals.*

/**
 * Provides scoping for {@link MethodReference}.
 */
class MethodReferenceScopes {
	
	@Inject extension IQualifiedNameConverter 
	
	/**
	 * Delegate to appropiate methods depending on the passed reference.
	 */
	def IScope getMethodReferenceScope(MethodReference context, EReference reference) {
		return switch (reference) {
			case METHOD_REFERENCE__OPERATION: context.scopeFor_MethodReference_operation 
			// TODO provide scope for type as well - maybe use IImportsConfiguration
			default: IScope.NULLSCOPE
		}
	}
	
	/**
	 * Provides scoping for MethodReference.operation.
	 */
	protected def IScope scopeFor_MethodReference_operation(MethodReference context) {
		val type = context.typeReference?.type
		if (type instanceof JvmDeclaredType) {
			val fixtureMethods = type.fixtureMethods
			return Scopes.scopeFor(fixtureMethods, [simpleName.toQualifiedName], IScope.NULLSCOPE)
		}
		return IScope.NULLSCOPE	
	}
	
	/**
	 * @return all public, non-static operations that are annotated with @FixtureMethod, inherited methods included.
	 */
	protected def Iterable<JvmOperation> getFixtureMethods(JvmDeclaredType type) {
		return type.allFeatures.filter(JvmOperation).filter[
			return (visibility == JvmVisibility.PUBLIC) && !isStatic && hasFixtureMethodAnnotation
		]
	}
	
	/**
	 * @return {@code true} if the operation is annotated with {@code org.testeditor.fixture.core.interaction.FixtureMethod}.
	 */
	protected def boolean hasFixtureMethodAnnotation(JvmOperation operation) {
		// TODO do we want a hard-coded reference here or a dependency on core-fixture?
		return operation.annotations.exists[annotation.qualifiedName == "org.testeditor.fixture.core.interaction.FixtureMethod"]
	}
	
}