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
package org.testeditor.rcp4.views.tests.tree

import javax.inject.Inject
import org.eclipse.xtext.junit4.util.ParseHelper
import org.testeditor.aml.AmlModel
import org.testeditor.aml.dsl.tests.AbstractTest
import org.testeditor.aml.ModelUtil

abstract class AbstractParserTest extends AbstractTest {

	@Inject protected ParseHelper<AmlModel> parser
	@Inject protected extension ModelUtil modelUtil

	protected def AmlModel parse(CharSequence input) {
		return parser.parse(input)
	}

}
