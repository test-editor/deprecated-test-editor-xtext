package org.testeditor.xmllibrary.dsl.importer.xml

import java.io.File
import java.io.StringReader
import java.net.URL
import javax.inject.Singleton
import javax.xml.bind.JAXBContext
import org.testeditor.xmllibrary.domain.action.ActionGroups
import org.testeditor.xmllibrary.domain.binding.TechnicalBindingTypes

/**
 * Provides helper methods for unmarshalling {@link ActionGroups}
 * and {@link TechnicalBindingTypes}.
 */
@Singleton
class JAXBHelper {

	val context = JAXBContext.newInstance(ActionGroups, TechnicalBindingTypes)
	
	def ActionGroups unmarshalActionGroups(File file) {
		val unmarshaller = context.createUnmarshaller
		return unmarshaller.unmarshal(file) as ActionGroups 
	}
	
	def ActionGroups unmarshalActionGroups(URL url) {
		val unmarshaller = context.createUnmarshaller
		return unmarshaller.unmarshal(url) as ActionGroups 
	}
	
	def ActionGroups unmarshalActionGroups(CharSequence charSequence) {
		val unmarshaller = context.createUnmarshaller
		return unmarshaller.unmarshal(new StringReader(charSequence.toString)) as ActionGroups 
	}
	
	def TechnicalBindingTypes unmarshalTechnicalBindings(File file) {
		val unmarshaller = context.createUnmarshaller
		return unmarshaller.unmarshal(file) as TechnicalBindingTypes 
	}
	
	def TechnicalBindingTypes unmarshalTechnicalBindings(URL url) {
		val unmarshaller = context.createUnmarshaller
		return unmarshaller.unmarshal(url) as TechnicalBindingTypes 
	}
	
	def TechnicalBindingTypes unmarshalTechnicalBindings(CharSequence charSequence) {
		val unmarshaller = context.createUnmarshaller
		return unmarshaller.unmarshal(new StringReader(charSequence.toString)) as TechnicalBindingTypes 
	}
	
}