package org.testeditor.aml.dsl.conversion

import org.eclipse.xtext.conversion.impl.AbstractLexerBasedConverter
import org.eclipse.xtext.nodemodel.INode
import org.eclipse.xtext.conversion.ValueConverterException

class TemplateVariableDefValueConverter extends AbstractLexerBasedConverter<String> {
	
	override toValue(String string, INode node) throws ValueConverterException {
		if (!string.isNullOrEmpty && string.startsWith("${") && string.endsWith("}")) {
			return string.substring(2, string.length - 1)
		}
		throw new ValueConverterException('''Could not convert: «string»''', node, null)
	}
	
	override protected toEscapedString(String value) {
		return '''${«value»}'''
	}
	
}