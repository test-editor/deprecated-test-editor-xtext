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
package org.testeditor.tcl.dsl.tests.parser

import com.google.inject.Inject
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import org.testeditor.dsl.common.testing.DslParseHelper

abstract class AbstractParserTest extends AbstractTclTest {

	@Inject protected extension ValidationTestHelper
	@Inject protected extension DslParseHelper parserHelper

}
