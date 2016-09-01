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

import org.eclipse.jface.action.ActionContributionItem
import org.eclipse.jface.action.IMenuManager
import org.eclipse.jface.viewers.ISelectionProvider
import org.eclipse.ui.IWorkbenchPage
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.navigator.CommonActionProvider
import org.eclipse.ui.navigator.ICommonActionExtensionSite
import org.testeditor.tcl.dsl.ui.util.TclInjectorProvider

class TEActionProvider extends CommonActionProvider {

	ISelectionProvider selectionProvider
	
	IWorkbenchPage page

	override init(ICommonActionExtensionSite aSite) {
		super.init(aSite)
		selectionProvider = aSite.viewSite.selectionProvider
		page = PlatformUI.workbench.activeWorkbenchWindow.activePage
	}

	override fillContextMenu(IMenuManager menu) {
		super.fillContextMenu(menu)
		val con = menu.find("org.eclipse.ui.RenameResourceAction") as ActionContributionItem
		menu.remove(con)
		val renameAction =new RenameAction(con.action,selectionProvider,page)
		val injector = new TclInjectorProvider().get
		injector.injectMembers(renameAction)
		menu.appendToGroup("group.reorganize", renameAction)
	}

}
