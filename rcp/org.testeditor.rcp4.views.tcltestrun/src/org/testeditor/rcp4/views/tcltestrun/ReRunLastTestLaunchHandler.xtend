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
package org.testeditor.rcp4.views.tcltestrun

import org.eclipse.e4.core.di.annotations.Execute
import javax.inject.Inject
import org.eclipse.e4.core.di.annotations.CanExecute
import org.eclipse.e4.core.contexts.IEclipseContext

class ReRunLastTestLaunchHandler {

	@Inject
	IEclipseContext context

	@Execute
	def reLaunchLastTestLaunch() {
		val tclLauncherUi = context.get(TclLauncherUi)
		val lastTestLaunch = context.get(LastTestLaunchInformation)
		tclLauncherUi.launchTest(lastTestLaunch.testCasesCommaList, lastTestLaunch.project,
			lastTestLaunch.launcher, lastTestLaunch.options)
	}

	@CanExecute
	def boolean canExecute() {
		return context.containsKey(LastTestLaunchInformation)
	}
	
}
