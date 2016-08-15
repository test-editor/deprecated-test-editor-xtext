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

import org.eclipse.ui.navigator.CommonActionProvider
import org.eclipse.jface.action.IMenuManager
import org.eclipse.jface.action.ActionContributionItem
import org.eclipse.ui.navigator.ICommonActionExtensionSite
import org.eclipse.jface.viewers.ISelectionProvider

class TEActionProvider extends CommonActionProvider {

	ISelectionProvider selectionProvider

	override init(ICommonActionExtensionSite aSite) {
		super.init(aSite)
		selectionProvider = aSite.viewSite.selectionProvider
	}

	override fillContextMenu(IMenuManager menu) {
		super.fillContextMenu(menu)
		val con = menu.find("org.eclipse.ui.RenameResourceAction") as ActionContributionItem
		menu.remove(con)
		menu.appendToGroup("group.reorganize", new RenameAction(con.action,selectionProvider))
	}

}
