package org.testeditor.tsl.dsl.conversion

import javax.inject.Inject
import org.eclipse.xtext.common.services.Ecore2XtextTerminalConverters
import org.eclipse.xtext.conversion.IValueConverter
import org.eclipse.xtext.conversion.ValueConverter

class TslValueConverterService extends Ecore2XtextTerminalConverters {
	
	@Inject TextValueConverter textValueConverter
	
	@ValueConverter(rule="Text")
	public def IValueConverter<String> TEMPLATE_VARIABLE_DEF() {
		return textValueConverter
	}
	
}