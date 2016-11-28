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
import org.eclipse.e4.ui.di.Focus

class TestExecutionLogViewPart {

	Browser browser
	
	String port

	@PostConstruct
	def void buildUI(Composite parent) {
		browser = new Browser(parent, SWT.NORMAL)
		port = System.getProperty("org.osgi.service.http.port")
		val url = '''http://localhost:«port»/testlogs/web-app/index.html'''
		browser.setUrl(url)
	}

	@Focus
	def void setFocus() {
		if (browser !== null) {
			browser.setFocus
		}
	}

	def void showLog(TestExecutionLog log) {
		val url = '''http://localhost:«port»/testlogs/web-app/index.html#/testrun/«log.testRunTimestamp»'''
		browser.setUrl(url)
	}

}
