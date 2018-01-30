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

import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IFolder
import org.eclipse.core.resources.IProject
import org.eclipse.jface.viewers.Viewer
import org.eclipse.jface.viewers.ViewerFilter

/** 
 * 
 * Hides all Folers direct located under the project. Test structures are located under the src enrty in the java project.
 * 
 */
class HideRootFolderFilter extends ViewerFilter {

	override select(Viewer viewer, Object parentElement, Object element) {
		if (element instanceof IFolder) {
			if (element.parent instanceof IProject) {
				return false
			}
		}
		if (element instanceof IFile) {
			if (element.parent instanceof IProject) {
				return false
			}
		}
		return true
	}

}
