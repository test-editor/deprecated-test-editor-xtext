/*******************************************************************************
 * Copyright (c) 2012 - 2015 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 *******************************************************************************/
package org.testeditor.aml.legacy.importer

import java.util.List
import javax.inject.Inject
import org.testeditor.aml.ModelElement
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
