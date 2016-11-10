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
package org.testeditor.rcp4.views.tcltestrun.model;

public enum TestElementType {
	SystemGroup("System"), TestComponentGroup("Test Component"), TestStepGroup("Test step"), TestSpecGroup(
			"Test specification");

	private String type;

	TestElementType(String name) {
		type = name;
	}

	public String getType() {
		return type;
	}
}