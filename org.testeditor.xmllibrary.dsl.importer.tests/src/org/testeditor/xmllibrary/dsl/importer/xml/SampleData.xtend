package org.testeditor.xmllibrary.dsl.importer.xml

import com.google.inject.Inject
import com.google.inject.Singleton
import org.testeditor.xmllibrary.domain.action.ActionGroups
import org.testeditor.xmllibrary.domain.binding.TechnicalBindingTypes

import static extension com.google.common.io.Resources.getResource

/**
 * Provides sample data for tests.
 */
@Singleton
class SampleData {
	
	@Inject
	JAXBHelper jaxb
	
	def TechnicalBindingTypes getBindings() {
		val url = "TechnicalBindingTypes.xml".resource	
		return jaxb.unmarshalTechnicalBindings(url)
	}
	
	def ActionGroups getActionGroups() {
		val url = "AllActionGroups.xml".resource
		return jaxb.unmarshalActionGroups(url)
	}
	
}