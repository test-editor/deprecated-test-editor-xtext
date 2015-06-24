package org.testeditor.aml.dsl.conversion

import javax.inject.Inject
import org.eclipse.xtext.common.services.Ecore2XtextTerminalConverters
import org.eclipse.xtext.conversion.IValueConverter
import org.eclipse.xtext.conversion.ValueConverter

class AmlValueConverterService extends Ecore2XtextTerminalConverters {

	@Inject
	private TemplateVariableDefValueConverter templateVariableValueConverter

	@ValueConverter(rule="TEMPLATE_VARIABLE_DEF")
	public def IValueConverter<String> TEMPLATE_VARIABLE_DEF() {
		return templateVariableValueConverter
	}

}