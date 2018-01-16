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
package org.testeditor.rcp4.views.tcltestrun

import org.eclipse.e4.core.contexts.IEclipseContext
import org.eclipse.e4.core.di.annotations.CanExecute
import org.eclipse.e4.core.di.annotations.Execute

class RerunLastTestLaunchHandler {

	@Execute
	def boolean rerunLastTestLaunch(IEclipseContext context) {
		val launcher = context.get(TclLauncherUi)
		val lastTestLaunch = context.get(TestLaunchInformation)
		launcher.launchTest(lastTestLaunch)
	}

	@CanExecute
	def boolean canExecute(IEclipseContext context) {
		return context.containsKey(TestLaunchInformation)
	}

}