package org.testeditor.aml.dsl.scoping

import com.google.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.common.types.JvmEnumerationLiteral
import org.eclipse.xtext.common.types.JvmEnumerationType
import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.eclipse.xtext.scoping.IScope
import org.eclipse.xtext.scoping.Scopes
import org.eclipse.xtext.xtype.XImportDeclaration
import org.testeditor.aml.AmlModel

class LocatorStrategyScopes {

	@Inject extension IQualifiedNameConverter

	def IScope getLocatorStrategyScope(EObject eObject) {
		val enumImports = getEnumImports(eObject)

		// First, create a scope for non-static imports
		val nonStaticImportedLiterals = enumImports.filter[!isStatic].enumLiterals
		val nonStaticScope = Scopes.scopeFor(nonStaticImportedLiterals, [
			return '''«declaringType.simpleName».«simpleName»'''.toString.toQualifiedName
		], IScope.NULLSCOPE)

		// Second, create a scope for static imports (they don't need the declaring type)
		val staticImportedLiterals = enumImports.filter[isStatic].enumLiterals
		return Scopes.scopeFor(staticImportedLiterals, [
			return simpleName.toQualifiedName
		], nonStaticScope)
	}

	// TODO improve this, we need a better way to determine "reachable" enums
	private def Iterable<XImportDeclaration> getEnumImports(EObject eObject) {
		val rootElement = EcoreUtil2.getRootContainer(eObject) as AmlModel
		val importSection = rootElement.importSection
		if (importSection !== null) {
			return rootElement.importSection.importDeclarations.filter[importedType instanceof JvmEnumerationType]
		} else {
			return #[]
		}
	}

	private def Iterable<JvmEnumerationLiteral> getEnumLiterals(Iterable<XImportDeclaration> imports) {
		return imports.map[importedType as JvmEnumerationType].map[literals].flatten
	}

}
