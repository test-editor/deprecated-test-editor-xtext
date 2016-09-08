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
package org.testeditor.rcp4.views.projectexplorer

import org.eclipse.core.runtime.IConfigurationElement
import org.eclipse.jface.action.IMenuManager
import org.eclipse.jface.action.MenuManager
import org.eclipse.ui.navigator.CommonActionProvider
import org.testeditor.dsl.common.util.PlatformHelper
import org.eclipse.jface.action.IAction
import org.eclipse.jface.action.Separator

class TEActionProvider extends CommonActionProvider {

	override fillContextMenu(IMenuManager menu) {
		super.fillContextMenu(menu)
		menu.items.forEach[println(it)]
		removeOpenWithMenu(menu)
		addLauncherActions(menu);
	}

	def private void addLauncherActions(IMenuManager menu) {
		val extReg = new PlatformHelper().extensionRegistry
		val launchers = extReg.getConfigurationElementsFor("org.testeditor.tcl.dsl.ui.tcl_launcher").filter(
			IConfigurationElement)
			menu.insertAfter("group.reorganize", new Separator("te.launch"))
		launchers.forEach [
			if (it.getAttribute("actionClass") != null) {
				menu.insertAfter("te.launch",it.createExecutableExtension("actionClass") as IAction)
			}
		]
	}

	def private void removeOpenWithMenu(IMenuManager menu) {
		val openWithMenues = menu.items.filter[it.id != null && it.id.equals("group.openWith")].filter(MenuManager)
		if (!openWithMenues.empty) {
			val openWithAction = openWithMenues.head
			menu.remove(openWithAction)
		}
	}

}
