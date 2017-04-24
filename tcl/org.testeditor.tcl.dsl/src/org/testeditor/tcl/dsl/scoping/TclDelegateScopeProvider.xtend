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

import org.eclipse.emf.ecore.EReference
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.scoping.IScope
import org.eclipse.xtext.scoping.impl.ScopeBasedSelectable
import org.eclipse.xtext.xbase.scoping.XImportSectionNamespaceScopeProvider
import org.testeditor.tcl.TclModel
import javax.inject.Inject
import org.testeditor.dsl.common.util.classpath.ClasspathUtil

public class TclDelegateScopeProvider extends XImportSectionNamespaceScopeProvider {

	@Inject ClasspathUtil classpathUtil

	override getResourceScope(IScope globalScope, Resource resource, EReference reference) {
		var IScope result = globalScope
		val globalScopeSelectable = new ScopeBasedSelectable(result)
		val normalizers = getImplicitImports(isIgnoreCase(reference))

		// Custom code START
		val head = resource.contents.head
		if (head instanceof TclModel) {
			if (head.package === null) {
				val package = classpathUtil.inferPackage(head)
				if (!package.nullOrEmpty) {
					head.package = package // create head.package only if present and not default package!
				}
			}
			val qualifiedName = head.qualifiedNameOfLocalElement
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

}
