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
package org.testeditor.tcl.dsl.ui.tests.quickfix

import org.junit.Test
import org.mockito.InjectMocks
import org.testeditor.tcl.dsl.ui.quickfix.TclQuickfixProvider
import org.testeditor.tcl.dsl.tests.AbstractTclTest

class TclQuickfixProviderTest extends AbstractTclTest {

	@InjectMocks TclQuickfixProvider quickFixProvider

	@Test
	def void testGetComponentDSLFragment() {
		// when
		val componentDslFragment = quickFixProvider.getComponentDSLFragment("Foo")

		// then
		val expected = '''
			component Foo is <TYPE> {

			}
		'''
		componentDslFragment.assertEquals(expected)
	}

}
