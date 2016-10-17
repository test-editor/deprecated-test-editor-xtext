/** 
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 */
package org.testeditor.rcp4.views.tcltestrun

import org.testeditor.dsl.common.testing.AbstractTest
import org.junit.Test

class StateLocationHelperTest extends AbstractTest {
	
	@Test
	def void testFileStoreExists() {
		//given
		val helper = new StateLocationHelper()
		
		//when
		val file = helper.stateLocation
		
		//then
		assertNotNull(file)
		assertTrue(file.exists)
		assertTrue(file.directory)
	}
	
}
