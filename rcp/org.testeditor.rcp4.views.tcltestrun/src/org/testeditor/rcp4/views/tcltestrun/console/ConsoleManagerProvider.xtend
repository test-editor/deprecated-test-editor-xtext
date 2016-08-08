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

import org.eclipse.ui.console.ConsolePlugin
import org.eclipse.ui.console.IConsoleManager

class ConsoleManagerProvider {
	
	def IConsoleManager getConsoleManager() {
		ConsolePlugin.getDefault().consoleManager
	}
	
}