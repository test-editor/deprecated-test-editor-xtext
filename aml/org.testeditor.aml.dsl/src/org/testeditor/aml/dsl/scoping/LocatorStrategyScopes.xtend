package org.testeditor.aml.dsl.scoping

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EReference
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.common.types.JvmEnumerationType
import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.eclipse.xtext.scoping.IScope
import org.eclipse.xtext.scoping.Scopes
import org.testeditor.aml.AmlModel

import static org.testeditor.aml.AmlPackage.Literals.*

class LocatorStrategyScopes {
	@Inject extension IQualifiedNameConverter

	def IScope getLocatorStrategyScope(EObject eObject, EReference reference) {
		return switch (reference) {
			case INTERACTION_TYPE__LOCATOR_STRATEGY,
			case COMPONENT_ELEMENT__LOCATOR_STRATEGY: eObject.scopeFor_InteractionType_locatorStrategy
			default: IScope.NULLSCOPE
		}
	}

	protected def IScope scopeFor_InteractionType_locatorStrategy(EObject eObject) {
		val rootElement = EcoreUtil2.getRootContainer(eObject) as AmlModel
		val importedTypes = rootElement.importSection.importDeclarations.map[importedType]
		val locatorEnums = importedTypes.filter(JvmEnumerationType)
		// additional filter applicable (e.g. naming convention or annotation)
		val literals = locatorEnums.map[literals].flatten
		return Scopes.scopeFor(literals, [simpleName.toQualifiedName], IScope.NULLSCOPE)
	}

}
