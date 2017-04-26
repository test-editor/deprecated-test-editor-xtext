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
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.naming.QualifiedName
import org.eclipse.xtext.scoping.IScope
import org.eclipse.xtext.scoping.impl.ScopeBasedSelectable
import org.eclipse.xtext.xbase.scoping.XImportSectionNamespaceScopeProvider
import org.testeditor.aml.AmlModel
import org.testeditor.dsl.common.util.classpath.ClasspathUtil

/**
 * Since we don't generate JvmTypes yet we need to tweak the scoping a little bit
 * here. We want to implicitly import all elements from our own packages as well.
 * 
 * This would usually provided by Xbase but only for JvmTypes, not for model-only types.
 */
class AmlDelegateScopeProvider extends XImportSectionNamespaceScopeProvider {

	@Inject ClasspathUtil classpathUtil

	override protected getResourceScope(IScope globalScope, Resource resource, EReference reference) {
		var IScope result = globalScope
		val globalScopeSelectable = new ScopeBasedSelectable(result)
		val normalizers = getImplicitImports(isIgnoreCase(reference))
		
		// Custom code START
		val head = resource.contents.head
		if (head instanceof AmlModel) {
			val qualifiedName = head.determineQualifiedName
			if (qualifiedName !== null) {
				normalizers += doCreateImportNormalizer(qualifiedName, true, false)
			}
		}
		// Custom code END
		if (!normalizers.isEmpty()) {
			result = createImportScope(result, normalizers, globalScopeSelectable, reference.getEReferenceType(),
				isIgnoreCase(reference))
		}
		return result
	}

	private def QualifiedName determineQualifiedName(AmlModel amlModel) {
		if (amlModel.package === null) {
			val derivedPackage = classpathUtil.inferPackage(amlModel)
			if (derivedPackage.nullOrEmpty) {
				return null // no qualified named can be derived
			} else {
				return QualifiedName.create(derivedPackage.split('\\.').toList)
			}
		}
		return amlModel.qualifiedNameOfLocalElement
	}

}
