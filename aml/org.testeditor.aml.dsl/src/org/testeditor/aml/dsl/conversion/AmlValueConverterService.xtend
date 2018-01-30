/*******************************************************************************
 * Copyright (c) 2012 - 2018 Signal Iduna Corporation and others.
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