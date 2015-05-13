package org.testeditor.xmllibrary.dsl.generator

import java.util.List
import org.eclipse.emf.ecore.resource.Resource
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
		fsa.generateFile(filename, compile(container))
	}
	
	def String compile(TechnicalBindingTypes container) {
		preprocessModel(container)
		
		val result = '''
			<?xml version="1.0" encoding="UTF-8" standalone="no"?>
			<TechnicalBindingTypes xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" schemaVersion="1.1">
				«FOR type : container.types»
					<TechnicalBindingType id="«type.id»" name="«type.name»">
						«type.actionParts.compile»
					</TechnicalBindingType>
				«ENDFOR»
			</TechnicalBindingTypes>
		'''
		
		return result
	}
	
	protected def String compile(List<ActionPart> actionParts) '''
		«FOR part : actionParts»
			<actionPart position="«part.position»" type="«part.type»"«part.handleTextValue»«part.handleArgumentId»/>
		«ENDFOR»
	'''
	
	def String handleTextValue(ActionPart actionPart) {
		if(actionPart.type == ActionType.TEXT) {
			return ''' value="«actionPart.value»"'''
		}
		
		return ""
	}	
	
	def String handleArgumentId(ActionPart actionPart) {
		if(actionPart.type == ActionType.ARGUMENT && !actionPart.id.nullOrEmpty) {
			return ''' id="«actionPart.id»"'''
		}
		
		return ""
	}

	def void preprocessModel(TechnicalBindingTypes container) {
		for (binding : container.types) {
			binding.actionParts.setPositions
			binding.actionParts.setEmptyTypesToText
		}
	}

	protected def void setPositions(List<ActionPart> actionParts) {
		actionParts.forEach [ part, i |
			part.position = i+1
		]
	}
	
	protected def void setEmptyTypesToText(List<ActionPart> actionParts) {
		actionParts.filter[type === null].forEach[type = ActionType.TEXT]
	}

}
