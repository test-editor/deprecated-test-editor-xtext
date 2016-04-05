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

import org.eclipse.jdt.core.IClasspathEntry
import org.eclipse.jface.viewers.ILabelProvider
import org.eclipse.jface.viewers.LabelProvider
import org.eclipse.ui.ISharedImages
import org.eclipse.ui.PlatformUI

/**
 * Label Provider to extend the Common Navigtor for test elements. It decorates src classpath entries as root elements for test definitions.
 */
class TELabelProvider extends LabelProvider implements ILabelProvider {

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
				val result = elementPath.substring(elementPath.indexOf("/", 1))
				if (result == '/src/test/java') {
					return "Tests"
				}
				if (result == '/src/main/java') {
					return "AML"
				}
				return result
			}
			return elementPath
		}
		return null
	}

}
