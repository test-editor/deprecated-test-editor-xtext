package org.testeditor.tml.dsl.scoping

import org.eclipse.emf.ecore.EReference
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.scoping.IScope
import org.eclipse.xtext.scoping.impl.ScopeBasedSelectable
import org.eclipse.xtext.xbase.scoping.XImportSectionNamespaceScopeProvider
import org.testeditor.tml.TmlModel

public class TmlDelegateScopeProvider extends XImportSectionNamespaceScopeProvider {

	override getResourceScope(IScope globalScope, Resource resource, EReference reference) {
		var IScope result = globalScope
		val globalScopeSelectable = new ScopeBasedSelectable(result)
		val normalizers = getImplicitImports(isIgnoreCase(reference))

		// Custom code START
		val head = resource.contents.head
		if (head instanceof TmlModel) {
			normalizers += doCreateImportNormalizer(head.qualifiedNameOfLocalElement, true, false)
		}
		// Custom code END
		if (!normalizers.isEmpty()) {
			result = createImportScope(result, normalizers, globalScopeSelectable, reference.getEReferenceType(),
				isIgnoreCase(reference))
		}
		return result
	}

}
