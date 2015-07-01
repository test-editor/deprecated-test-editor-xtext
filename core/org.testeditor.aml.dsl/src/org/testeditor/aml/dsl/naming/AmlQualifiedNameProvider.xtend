package org.testeditor.aml.dsl.naming

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.eclipse.xtext.naming.SimpleNameProvider
import org.testeditor.aml.model.InteractionType
import org.testeditor.aml.model.TemplateVariable

class AmlQualifiedNameProvider extends SimpleNameProvider {

	@Inject
	extension IQualifiedNameConverter

	override getFullyQualifiedName(EObject obj) {
		if (obj instanceof TemplateVariable) {
			return getfullyQualifiedNameFor(obj)
		}
		return super.getFullyQualifiedName(obj)
	}

	protected def getfullyQualifiedNameFor(TemplateVariable variable) {
		if (variable.name == "element") {
			return null // shall not be referenced from the outside world
		}
		val container = variable.eContainer?.eContainer
		if (container instanceof InteractionType) {
			return '''«container.name».«variable.name»'''.toString.toQualifiedName
		}
		return null
	}

}