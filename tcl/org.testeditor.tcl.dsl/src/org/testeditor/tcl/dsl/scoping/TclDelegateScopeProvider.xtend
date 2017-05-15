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
package org.testeditor.tcl.dsl.scoping

import javax.inject.Inject
import org.eclipse.emf.ecore.EReference
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.naming.QualifiedName
import org.eclipse.xtext.scoping.IScope
import org.eclipse.xtext.scoping.impl.ScopeBasedSelectable
import org.eclipse.xtext.xbase.scoping.XImportSectionNamespaceScopeProvider
import org.testeditor.dsl.common.util.classpath.ClasspathUtil
import org.testeditor.tcl.TclModel

public class TclDelegateScopeProvider extends XImportSectionNamespaceScopeProvider {

	@Inject ClasspathUtil classpathUtil

	override getResourceScope(IScope globalScope, Resource resource, EReference reference) {
		var IScope result = globalScope
		val globalScopeSelectable = new ScopeBasedSelectable(result)
		val normalizers = getImplicitImports(isIgnoreCase(reference))

		// Custom code START
		val head = resource.contents.head
		if (head instanceof TclModel) {
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

	private def QualifiedName determineQualifiedName(TclModel tclModel) {
		if (tclModel.package === null) {
			val derivedPackage = classpathUtil.inferPackage(tclModel)
			if (derivedPackage.nullOrEmpty) {
				return null // no qualified named can be derived
			} else {
				return QualifiedName.create(derivedPackage.split('\\.').toList)
			}
		}
		return tclModel.qualifiedNameOfLocalElement
	}

}
