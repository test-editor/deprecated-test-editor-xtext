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
package org.testeditor.tsl.dsl.parser

import javax.inject.Inject
import org.eclipse.xtext.junit4.util.ParseHelper
import org.testeditor.tsl.TslModel
import org.testeditor.tsl.dsl.tests.AbstractTest

abstract class AbstractParserTest extends AbstractTest {

	@Inject protected ParseHelper<TslModel> parser

	protected def TslModel parse(CharSequence input) {
		return parser.parse(input)
	}
	
	protected def TslModel parse(CharSequence input, (TslModel)=>void assertionBlock) {
		val model = input.parse
		assertionBlock.apply(model)
		return model
	}

}