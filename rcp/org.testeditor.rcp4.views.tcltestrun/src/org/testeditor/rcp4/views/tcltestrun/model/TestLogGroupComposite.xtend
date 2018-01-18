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
package org.testeditor.rcp4.views.tcltestrun.model

import java.util.List
import org.eclipse.xtend.lib.annotations.Accessors

class TestLogGroupComposite extends TestLogGroup {
	
	@Accessors(PUBLIC_GETTER)
	List<TestLogGroup> children = newArrayList
	
	new(TestElementType type) {
		super(type)
	}
		
	def void add(TestLogGroup testLogGroup) {
		testLogGroup.setParent(this)
		children.add(testLogGroup)
	}
		
}