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
package org.testeditor.tcl.dsl.tests.parser

import javax.inject.Inject
import org.eclipse.xtext.junit4.util.ParseHelper
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.dsl.tests.AbstractTest
import org.testeditor.tcl.util.TclModelUtil

abstract class AbstractParserTest extends AbstractTest {

	@Inject protected ParseHelper<TclModel> parser
	@Inject protected extension TclModelUtil modelUtil

	protected def TestCase parse(CharSequence input) {
		return parser.parse(input).test
	}

}