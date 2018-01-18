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