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
package org.testeditor.rcp4

import org.eclipse.e4.core.contexts.ContextInjectionFactory
import org.eclipse.e4.core.contexts.IEclipseContext
import org.eclipse.e4.core.di.annotations.Execute
import org.eclipse.jface.action.Action
import org.eclipse.jface.action.IMenuManager
import org.eclipse.swt.SWT
import org.eclipse.ui.IWorkbenchWindow
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.application.ActionBarAdvisor
import org.eclipse.ui.application.IActionBarConfigurer
import org.testeditor.rcp4.handlers.OpenNetworkConfigurationHandler

/** dummy class */
class ApplicationActionBarAdvisor extends ActionBarAdvisor {
	new(IActionBarConfigurer configurer) {
		super(configurer)
	}

	override makeActions(IWorkbenchWindow window) {
	}

	override fillMenuBar(IMenuManager menuBar) {
		menuBar.add(new Action("&NetworkConfig",SWT.NORMAL){
			
			override run() {
				val context = PlatformUI.getWorkbench().getService(IEclipseContext);
				val handler = ContextInjectionFactory.make(OpenNetworkConfigurationHandler,context)
				ContextInjectionFactory.invoke(handler,Execute,context)
			}
			
		})
	}
}
