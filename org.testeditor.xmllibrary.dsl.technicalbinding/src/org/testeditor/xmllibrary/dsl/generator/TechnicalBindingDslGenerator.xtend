package org.testeditor.xmllibrary.dsl.generator

import java.util.List
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.xmi.impl.XMLResourceImpl
import org.eclipse.emf.ecore.xmi.util.XMLProcessor
import org.eclipse.xtext.generator.IFileSystemAccess
import org.eclipse.xtext.generator.IGenerator
import org.testeditor.xmllibrary.model.ActionPart
import org.testeditor.xmllibrary.model.ActionType
import org.testeditor.xmllibrary.model.TechnicalBindingTypes

class TechnicalBindingDslGenerator implements IGenerator {

	override void doGenerate(Resource resource, IFileSystemAccess fsa) {
		val container = resource.contents.head as TechnicalBindingTypes
		val sourceFilename = resource.URI.trimFragment.trimFileExtension.lastSegment
		val filename = '''«sourceFilename».xml'''.toString
		fsa.generateFile(filename, compile(container, URI.createURI(filename)))
	}
	
	def String compile(TechnicalBindingTypes container, URI uri) {
		preprocessModel(container)
		val resource = new XMLResourceImpl(uri) => [
			encoding = "UTF-8"
			contents += container
		]
		val processor = new XMLProcessor
		return processor.saveToString(resource, null)
	}

	def void preprocessModel(TechnicalBindingTypes container) {
		for (binding : container.types) {
			binding.actionParts.setPositions
			binding.actionParts.setEmptyTypesToText
		}
	}

	protected def void setPositions(List<ActionPart> actionParts) {
		actionParts.forEach [ part, i |
			part.position = i
		]
	}
	
	protected def void setEmptyTypesToText(List<ActionPart> actionParts) {
		actionParts.filter[type === null].forEach[type = ActionType.TEXT]
	}

}
