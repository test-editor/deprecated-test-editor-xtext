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
import org.eclipse.e4.ui.model.application.MApplication
import org.eclipse.e4.ui.model.application.ui.basic.MTrimmedWindow
import org.eclipse.jface.action.Action
import org.eclipse.jface.action.ICoolBarManager
import org.eclipse.jface.action.IMenuManager
import org.eclipse.jface.action.MenuManager
import org.eclipse.swt.SWT
import org.eclipse.ui.IWorkbenchWindow
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.application.ActionBarAdvisor
import org.eclipse.ui.application.IActionBarConfigurer
import org.testeditor.dsl.common.util.EclipseContextHelper
import org.testeditor.rcp4.handlers.OpenNetworkConfigurationHandler
import org.testeditor.rcp4.handlers.RestartAndResetUIHandler

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

	def void removeUnwantedMenus() {
		mainMenu.items.filter[it != configMenu].forEach[visible = false]
		val contextHelper = new EclipseContextHelper()
		val context = contextHelper.eclipseContext
		val mApplication = context.getParent().get(MApplication)
		val coolBarItems = mApplication.children.filter(MTrimmedWindow).head.trimBars.filter [
			elementId.equals("org.eclipse.ui.main.toolbar")
		].head.children
		coolBarItems.filter [
			!(elementId.startsWith("org.testeditor") ||
				elementId.equals("org.eclipse.ui.edit.text.actionSet.navigation"))
		].forEach[toBeRendered = false]
	}

}
