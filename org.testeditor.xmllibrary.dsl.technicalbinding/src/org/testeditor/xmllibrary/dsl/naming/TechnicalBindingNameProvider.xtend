package org.testeditor.xmllibrary.dsl.naming

import com.google.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.eclipse.xtext.naming.SimpleNameProvider
import org.testeditor.xmllibrary.model.ActionPart
import org.testeditor.xmllibrary.model.TechnicalBindingType

class TechnicalBindingNameProvider extends SimpleNameProvider {

	@Inject
	extension IQualifiedNameConverter qualifiedNameConverter

	override getFullyQualifiedName(EObject obj) {
		if (obj instanceof TechnicalBindingType) {
			return obj.id.toQualifiedName
		}
		if (obj instanceof ActionPart) {
			if (!obj.id.nullOrEmpty) {
				return obj.id.toQualifiedName
			}
		}
		return super.getFullyQualifiedName(obj)
	}

}