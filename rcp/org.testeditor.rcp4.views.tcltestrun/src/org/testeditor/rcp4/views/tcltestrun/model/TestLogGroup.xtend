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
package org.testeditor.rcp4.views.tcltestrun.model

import java.util.List
import java.util.ArrayList
import org.eclipse.xtend.lib.annotations.Accessors

class TestLogGroup {

	@Accessors
	TestLogGroupComposite parent;

	@Accessors
	TestElementType type

	@Accessors
	private String name;

	List<String> logentries = new ArrayList<String>

	new(TestElementType type) {
		this.type = type
	}

	def addLogLine(String logLine) {
		if (!logLine.trim.empty) {
			logentries.add(logLine);
		}
	}

	def List<String> getLogLines() {
		return logentries
	}

	override toString() {
		return type.name + ": " + name
	}

}
