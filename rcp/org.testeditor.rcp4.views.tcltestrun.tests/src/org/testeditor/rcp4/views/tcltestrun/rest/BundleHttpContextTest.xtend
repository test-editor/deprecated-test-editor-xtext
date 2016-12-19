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
package org.testeditor.rcp4.views.tcltestrun.rest

import org.junit.Test
import org.mockito.Mock
import org.osgi.framework.Bundle
import org.testeditor.dsl.common.testing.AbstractTest

import static org.mockito.Mockito.*

class BundleHttpContextTest extends AbstractTest {
	@Mock
	Bundle bundle
	
	@Test
	def void testGetResourceWithLeadingSlash() {
		// given
		val ctx = new BundleHttpContext(bundle)
		
		// when
		ctx.getResource("/foo.txt")
		
		// then
		verify(bundle).getEntry("foo.txt")		
	}
	
	@Test
	def void testGetResourceWithOutLeadingSlash() {
		// given
		val ctx = new BundleHttpContext(bundle)
		
		// when
		ctx.getResource("foo.txt")
		
		// then
		verify(bundle).getEntry("foo.txt")				
	}
	
}