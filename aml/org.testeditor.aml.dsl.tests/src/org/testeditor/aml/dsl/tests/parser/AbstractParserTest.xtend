/*******************************************************************************
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
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
package org.testeditor.aml.dsl.tests.parser

import javax.inject.Inject
import org.eclipse.xtext.junit4.util.ParseHelper
import org.testeditor.aml.dsl.tests.AbstractTest
import org.testeditor.aml.AmlModel
import org.testeditor.aml.ModelElement

abstract class AbstractParserTest extends AbstractTest {
	
	@Inject
	protected ParseHelper<AmlModel> parser

	protected def AmlModel parse(CharSequence input) {
		return parser.parse(input)
	}
	
	/**
	 * Creates a sample model by adding a package definition to the passed input, i.e.
	 * <pre>
	 * 	package com.example
	 * 	«input»
	 * </pre>
	 * and returns the first element of the passed type within the model's eAllContents.
	 * 
	 * @param the partial model to parse
	 * @param elementClass the expected type
	 * @return {@code model.eAllContents.filter(elementClass).head}
	 */
	protected def <T extends ModelElement> T parse(CharSequence input, Class<T> elementClass) {
		val newInput = '''
			package com.example
			
			«input»
		'''
		val model = parser.parse(newInput)
		return model.eAllContents.filter(elementClass).head
	}

}