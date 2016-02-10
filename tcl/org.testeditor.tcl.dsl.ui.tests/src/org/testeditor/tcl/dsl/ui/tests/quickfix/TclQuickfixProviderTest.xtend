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
package org.testeditor.tcl.dsl.ui.tests.quickfix

import org.junit.Test
import org.testeditor.tcl.dsl.ui.quickfix.TclQuickfixProvider
import org.testeditor.tcl.dsl.tests.AbstractTest

class TclQuickfixProviderTest extends AbstractTest{
	
	@Test
	def void testGetComponentDSLFragment() {
		val qProv = new TclQuickfixProvider()
		
		val result = '''
			component Foo is <TYPE> {
			
			}
		'''
		//expect
		qProv.getComponentDSLFragment("Foo").trim.assertEquals(result.trim)
	}
		
}