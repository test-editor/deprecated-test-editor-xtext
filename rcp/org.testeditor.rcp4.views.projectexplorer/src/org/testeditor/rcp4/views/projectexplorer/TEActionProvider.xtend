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
package org.testeditor.rcp4.views.projectexplorer

import org.eclipse.core.runtime.IConfigurationElement
import org.eclipse.jface.action.IMenuManager
import org.eclipse.jface.action.MenuManager
import org.eclipse.ui.navigator.CommonActionProvider
import org.testeditor.dsl.common.util.PlatformHelper
import org.eclipse.jface.action.IAction
import org.eclipse.jface.action.Separator
import javax.inject.Inject

class TEActionProvider extends CommonActionProvider {

	@Inject PlatformHelper platformHelper

	override fillContextMenu(IMenuManager menu) {
		super.fillContextMenu(menu)
		removeOpenWithMenu(menu)
		addLauncherActions(menu)
	}

	def private void addLauncherActions(IMenuManager menu) {
		val extReg = platformHelper.extensionRegistry
		val launchers = extReg.getConfigurationElementsFor("org.testeditor.tcl.dsl.ui.tcl_launcher").filter(
			IConfigurationElement)
		menu.insertAfter("group.reorganize", new Separator("te.launch"))
		launchers.forEach [
			it.children.filter[it.name.equals("LaunchAction")].forEach [
				val action = it.createExecutableExtension("class") as IAction
				action.text = it.getAttribute("name")
				menu.insertAfter("te.launch", action)
			]
		]
	}

	def private void removeOpenWithMenu(IMenuManager menu) {
		val openWithMenues = menu.items.filter[id != null && id.equals("group.openWith")].filter(MenuManager)
		openWithMenues.forEach[menu.remove(it)]
	}

}
