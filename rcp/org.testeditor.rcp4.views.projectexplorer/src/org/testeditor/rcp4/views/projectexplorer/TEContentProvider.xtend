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

import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.jdt.core.IClasspathEntry
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jface.viewers.ITreeContentProvider
import org.eclipse.jface.viewers.Viewer

/**
 * Content Provider to extend the cnf navigator with the TE specific elements. The provider replaces folders of the classpath entry with one entry.
 */
class TEContentProvider implements ITreeContentProvider {

	override getChildren(Object parentElement) {
		if (parentElement instanceof IProject) {
			if (parentElement.hasNature(JavaCore.NATURE_ID)) {
				val javaProject = JavaCore.create(parentElement);
				return javaProject.rawClasspath.filter[entryKind == IClasspathEntry.CPE_SOURCE]
			}
		}
		if (parentElement instanceof IClasspathEntry) {
			return ResourcesPlugin.workspace.root.getFolder(parentElement.path).members
		}
		return null;
	}

	override getElements(Object inputElement) {
		return null
	}

	override getParent(Object element) {
	}

	override hasChildren(Object element) {
		if (element instanceof IClasspathEntry) {
			return ResourcesPlugin.workspace.root.getFolder(element.path).members.length > 0
		}
		return false
	}

	override dispose() {
	}

	override inputChanged(Viewer viewer, Object oldInput, Object newInput) {
	}

}
