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
import org.eclipse.jface.action.MenuManager
import org.eclipse.swt.SWT
import org.eclipse.ui.IWorkbenchWindow
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.application.ActionBarAdvisor
import org.eclipse.ui.application.IActionBarConfigurer
import org.testeditor.rcp4.handlers.OpenNetworkConfigurationHandler
import org.testeditor.rcp4.handlers.RestartAndResetUIHandler
import org.eclipse.jface.action.ICoolBarManager

/** dummy class */
class ApplicationActionBarAdvisor extends ActionBarAdvisor {

	IMenuManager mainMenu
	IMenuManager configMenu
	ICoolBarManager toolBar

	new(IActionBarConfigurer configurer) {
		super(configurer)
	}

	override makeActions(IWorkbenchWindow window) {
	}

	override fillMenuBar(IMenuManager menuBar) {
		mainMenu = menuBar
		configMenu = new MenuManager("&Configuration")
		menuBar.add(configMenu)
		configMenu.add(createActionFor("&NetworkConfig", OpenNetworkConfigurationHandler))
		configMenu.add(createActionFor("&Reset UI", RestartAndResetUIHandler))
	}

	override protected fillCoolBar(ICoolBarManager coolBar) {
		toolBar = coolBar
	}

	def Action createActionFor(String actionLabel, Class<?> hanlderClass) {
		return new Action(actionLabel, SWT.NORMAL) {

			override run() {
				val context = PlatformUI.getWorkbench().getService(IEclipseContext);
				val handler = ContextInjectionFactory.make(hanlderClass, context)
				ContextInjectionFactory.invoke(handler, Execute, context)
			}

		}
	}

	def removeUnwantedMenus() {
		mainMenu.items.filter[it != configMenu].forEach[visible = false]
		val hiddenToolBarEntries = #[
			"additions",
			"org.eclipse.search.searchActionSet",
			"org.eclipse.ui.edit.text.actionSet.annotationNavigation"
			//Can't remove external launcher without side effects (npe loop)
		]
		toolBar.items.filter[hiddenToolBarEntries.contains(it.id)].forEach[it.dispose]
	}

}
