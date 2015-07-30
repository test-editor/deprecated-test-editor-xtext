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
package org.testeditor.aml.dsl.scoping

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EReference
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.eclipse.xtext.scoping.IScope
import org.eclipse.xtext.scoping.Scopes
import org.eclipse.xtext.xbase.scoping.batch.XbaseBatchScopeProvider
import org.testeditor.aml.model.ElementTypeWithInteractions
import org.testeditor.aml.model.ElementWithInteractions
import org.testeditor.aml.model.ModelUtil
import org.testeditor.aml.model.TemplateVariable

// TODO this is a temporary scoping as long as we don't properly use Xbase
class AmlScopeProvider extends XbaseBatchScopeProvider {
	
	@Inject extension ModelUtil
	@Inject IQualifiedNameProvider nameProvider
	
	override getScope(EObject context, EReference reference) {
		if (context instanceof ElementWithInteractions<?>) {
			return context.interactionsScope
		}
		super.getScope(context, reference)
	}
	
	/**
	 * Provides the proper scope for template variables.
	 */
	def IScope getInteractionsScope(ElementWithInteractions<?> element) {
		if (element.type === null) {
			return IScope.NULLSCOPE
		}
		val variables = element.type.templateVariablesInScope
		return Scopes.scopeFor(variables, nameProvider, IScope.NULLSCOPE)
	}
	
	/**
	 * @return the {@link TemplateVariable variables} that can be referenced from the passed type
	 */
	protected def Iterable<TemplateVariable> getTemplateVariablesInScope(ElementTypeWithInteractions type) {
		val templates = type.interactionTypes.map[template].filterNull
		val variables = templates.map[referenceableVariables].flatten
		return variables
	}
	
}