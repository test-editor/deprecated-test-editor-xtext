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

import org.eclipse.jface.action.Action
import org.eclipse.jface.action.IAction
import org.eclipse.jface.viewers.ISelectionProvider
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.core.resources.IFile

class RenameAction extends Action {

	IAction resourceReanemAction
	ISelectionProvider selectionProvider

	new(IAction fileRenameAction, ISelectionProvider provider) {
		super("Rename...")
		resourceReanemAction = fileRenameAction
		selectionProvider = provider
	}

	override run() {
		val selection = selectionProvider.selection
		if (selection instanceof IStructuredSelection) {
			val selectedElement = selection.firstElement
			if (selectedElement instanceof IFile) {
				if (selectedElement.fileExtension.equals("tcl")) {
					println("xText Rename")
				} else {
					resourceReanemAction.run
				}
			} else {
				resourceReanemAction.run
			}
		}
	}

}
