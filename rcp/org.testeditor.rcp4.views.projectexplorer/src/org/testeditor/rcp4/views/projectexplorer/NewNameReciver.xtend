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

import org.eclipse.jface.dialogs.Dialog
import org.eclipse.jface.dialogs.InputDialog
import org.eclipse.swt.widgets.Display

class NewNameReciver {
	
	def String getNewName(String currentName) {
		val shell = Display.current.activeShell
		val inputDialog = new InputDialog(shell, "Rename","New name:",currentName, null)
		if(inputDialog.open == Dialog.OK) {
			return inputDialog.value
		}
		return null
	}
	
}