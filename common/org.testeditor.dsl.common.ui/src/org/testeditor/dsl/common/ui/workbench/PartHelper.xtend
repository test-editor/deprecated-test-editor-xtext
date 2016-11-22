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
package org.testeditor.dsl.common.ui.workbench

import java.util.Optional
import org.eclipse.core.resources.IFile
import org.eclipse.ui.IEditorPart
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.part.FileEditorInput

class PartHelper {

	def Optional<IEditorPart> findEditor(IFile input) {
		val editorInput = new FileEditorInput(input)
		val editor = PlatformUI.workbench.workbenchWindows.map[activePage.findEditor(editorInput)].head
		return Optional.ofNullable(editor)
	}

	def void showView(String viewId) {
		PlatformUI.workbench.display.syncExec [
			val window = PlatformUI.workbench.workbenchWindows.head
			window.activePage.showView(viewId)
		]
	}

	def findPart(String viewId) {
		val window = PlatformUI.workbench.workbenchWindows.head
		return window.activePage.findView(viewId)
	}

}
