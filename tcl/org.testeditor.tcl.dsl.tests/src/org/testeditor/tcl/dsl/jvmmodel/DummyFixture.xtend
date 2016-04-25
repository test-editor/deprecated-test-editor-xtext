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
package org.testeditor.tcl.dsl.jvmmodel

import java.util.List
import java.util.Map
import org.testeditor.fixture.core.interaction.FixtureMethod

class DummyFixture {
	@FixtureMethod
	def void startApplication(String name) {
	}

	@FixtureMethod
	def void stopApplication() {
	}

	@FixtureMethod
	def String getValue(String locator) {
		return ""
	}

	@FixtureMethod
	def void setValue(String locator, String value) {
	}
	
    @FixtureMethod
    def List<?> getList(String locator) {
    	return newArrayList
    }
    
    @FixtureMethod
    def Map<?,?> getMap(String locator){
    	return newHashMap
    }    
}