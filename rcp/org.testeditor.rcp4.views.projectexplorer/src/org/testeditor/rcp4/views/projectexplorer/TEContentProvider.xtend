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

import javax.inject.Inject
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.jdt.core.IClasspathEntry
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jface.viewers.ITreeContentProvider
import org.eclipse.jface.viewers.Viewer

/**
 * Content Provider to extend the cnf navigator with the TE specific elements. The provider replaces folders of the classpath entry with one entry.
 */
class TEContentProvider implements ITreeContentProvider {

	@Inject WorkspaceRootHelper workspaceRootHelper
	@Inject JavaCoreHelper javaCoreHelper

	override getChildren(Object parentElement) {
		if (parentElement instanceof IProject) {
			if (parentElement.hasNature(JavaCore.NATURE_ID)) {
				val javaProject = javaCoreHelper.create(parentElement);
				return javaProject.rawClasspath.filter[entryKind == IClasspathEntry.CPE_SOURCE]
			}
		}
		if (parentElement instanceof IClasspathEntry) {
			return workspaceRootHelper.getRoot.getFolder(parentElement.path).members
		}
		return null;
	}

	override getElements(Object inputElement) {
		return null
	}

	override getParent(Object element) {
		if (element instanceof IResource) {
			val classpathEntry = getParentClasspathEntry(element)
			if (classpathEntry != null) {
				return classpathEntry
			}
		}
		if (element instanceof IClasspathEntry) {
			return workspaceRootHelper.getRoot.getFolder(element.path).project
		}
		return null
	}

	/**
	 * returns the parent folder as ClasspathEntry or null if it is not a ClasspathEntry.
	 */
	def private IClasspathEntry getParentClasspathEntry(IResource element) {
		if (element.project != null) {
			val parentClasspathEntries = getChildren(element.project).filter(IClasspathEntry).filter [
				workspaceRootHelper.getRoot.getFolder(it.path).members.contains(element)
			]
			return parentClasspathEntries.head
		}
		return null
	}

	override hasChildren(Object element) {
		if (element instanceof IClasspathEntry) {
			return workspaceRootHelper.getRoot.getFolder(element.path).members.length > 0
		}
		return false
	}

	override dispose() {
	}

	override inputChanged(Viewer viewer, Object oldInput, Object newInput) {
	}

}
