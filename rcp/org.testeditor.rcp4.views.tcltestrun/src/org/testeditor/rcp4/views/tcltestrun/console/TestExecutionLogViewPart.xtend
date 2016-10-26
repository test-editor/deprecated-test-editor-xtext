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
package org.testeditor.rcp4.views.tcltestrun.console

import javax.annotation.PostConstruct
import org.eclipse.swt.SWT
import org.eclipse.swt.browser.Browser
import org.eclipse.swt.widgets.Composite
import org.testeditor.rcp4.views.tcltestrun.model.TestExecutionLog

class TestExecutionLogViewPart {
	
	Browser browser
	
	@PostConstruct
	def buildUI(Composite parent) {
		browser = new Browser(parent, SWT.NORMAL)
	}
	
	def showLog(TestExecutionLog log) {
		browser.setUrl("http://localhost:19090/services/testexeclog/list")
	}
	
}