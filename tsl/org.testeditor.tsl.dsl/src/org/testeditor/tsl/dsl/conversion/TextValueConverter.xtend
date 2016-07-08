package org.testeditor.tsl.dsl.conversion

import org.eclipse.xtext.conversion.ValueConverterException
import org.eclipse.xtext.conversion.impl.AbstractValueConverter
import org.eclipse.xtext.nodemodel.INode

class TextValueConverter extends AbstractValueConverter<String> {

	override toValue(String string, INode node) throws ValueConverterException {
		return string.trim
	}

	override toString(String value) throws ValueConverterException {
		return value
	}

}
