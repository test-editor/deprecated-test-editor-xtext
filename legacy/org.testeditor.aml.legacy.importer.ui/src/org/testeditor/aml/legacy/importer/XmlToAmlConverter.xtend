package org.testeditor.aml.legacy.importer

import java.util.List
import javax.inject.Inject
import org.testeditor.aml.model.ModelElement
import org.testeditor.xmllibrary.domain.util.XmlHelper

class XmlToAmlConverter {

	@Inject
	XmlHelper xmlHelper

	@Inject
	TechnicalBindingConverter technicalBindingConverter

	def List<? extends ModelElement> convert(String xml) {
		if (xml.contains("<TechnicalBindingTypes")) {
			return xml.handleTechnicalBindings
		} else if (xml.contains("<TechnicalBindingType")) {
			// does not contain the root element, we wrap it around it
			val newXml = '''
				<TechnicalBindingTypes>
				«xml»
				</TechnicalBindingTypes>
			'''
			return newXml.handleTechnicalBindings
		}

		throw new IllegalArgumentException('''
			Could not convert clipboard contents:
			
			«xml»''')
	}

	protected def List<? extends ModelElement> handleTechnicalBindings(String xml) {
		val technicalBindings = xmlHelper.unmarshalTechnicalBindings(xml)
		return technicalBindingConverter.convert(technicalBindings)
	}

}