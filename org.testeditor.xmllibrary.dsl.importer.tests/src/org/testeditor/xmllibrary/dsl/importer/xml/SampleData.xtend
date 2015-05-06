package org.testeditor.xmllibrary.dsl.importer.xml

import com.google.common.io.Resources
import com.google.inject.Inject
import com.google.inject.Singleton
import java.io.File
import org.testeditor.xmllibrary.domain.action.ActionGroups
import org.testeditor.xmllibrary.domain.binding.TechnicalBindingTypes

/**
 * Provides sample data for tests.
 */
@Singleton
class SampleData {
	
	@Inject
	JAXBHelper jaxb
	
	def File asFile(String path) {
		val url = Resources.getResource(path)
		new File(url.toURI)
	}
	
	def TechnicalBindingTypes getBindings() {
		val file = "TechnicalBindingTypes.xml".asFile	
		return jaxb.unmarshalTechnicalBindings(file)
	}
	
	def ActionGroups getActionGroups() {
		val file = "AllActionGroups.xml".asFile
		return jaxb.unmarshalActionGroups(file)
	}
	
}