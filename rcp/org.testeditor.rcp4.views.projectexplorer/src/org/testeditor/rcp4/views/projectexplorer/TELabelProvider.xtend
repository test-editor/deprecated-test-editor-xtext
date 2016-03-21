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

import org.eclipse.jface.viewers.ILabelProvider
import org.eclipse.jface.viewers.ILabelProviderListener
import org.eclipse.jdt.core.IClasspathEntry
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.ISharedImages

/**
 * Label Provider to extend the Common Navigtor for test elements. It decorates src classpath entries as root elements for test definitions.
 */
class TELabelProvider implements ILabelProvider {

	override getImage(Object element) {
		if (element instanceof IClasspathEntry) {
			return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER)
		}
		return null
	}

	override getText(Object element) {
		if (element instanceof IClasspathEntry) {
			val elementPath = element.path.toString
			if (elementPath.lastIndexOf("/") > 0) {
				return elementPath.substring(elementPath.indexOf("/", 1))
			}
			return elementPath
		}
		return null
	}

	override addListener(ILabelProviderListener listener) {
	}

	override dispose() {
	}

	override isLabelProperty(Object element, String property) {
		return false
	}

	override removeListener(ILabelProviderListener listener) {
	}

}
