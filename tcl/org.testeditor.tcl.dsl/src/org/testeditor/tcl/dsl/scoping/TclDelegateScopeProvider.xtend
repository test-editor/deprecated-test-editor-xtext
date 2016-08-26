package org.testeditor.tcl.dsl.scoping

import org.eclipse.emf.ecore.EReference
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.scoping.IScope
import org.eclipse.xtext.scoping.impl.ScopeBasedSelectable
import org.eclipse.xtext.xbase.scoping.XImportSectionNamespaceScopeProvider
import org.testeditor.tcl.TclModel
import javax.inject.Inject
import org.testeditor.dsl.common.util.ClasspathUtil

public class TclDelegateScopeProvider extends XImportSectionNamespaceScopeProvider {

	@Inject ClasspathUtil classpathUtil

	override getResourceScope(IScope globalScope, Resource resource, EReference reference) {
		var IScope result = globalScope
		val globalScopeSelectable = new ScopeBasedSelectable(result)
		val normalizers = getImplicitImports(isIgnoreCase(reference))

		// Custom code START
		val head = resource.contents.head
		if (head instanceof TclModel) {
			if(head.package == null){
				head.package = classpathUtil.inferPackage(head)
			} 
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
