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
import org.testeditor.dsl.common.util.WorkspaceHelper

/**
 * Content Provider to extend the cnf navigator with the TE specific elements. The provider replaces folders of the classpath entry with one entry.
 */
class TEContentProvider implements ITreeContentProvider {

	@Inject WorkspaceHelper workspaceHelper
	@Inject JavaCoreHelper javaCoreHelper

	override getChildren(Object parentElement) {
		if (parentElement instanceof IProject) {
			if (parentElement.hasNature(JavaCore.NATURE_ID)) {
				val javaProject = javaCoreHelper.create(parentElement);
				return javaProject.rawClasspath.filter[isRelevantClasspathEntry]
			}
		}
		if (parentElement instanceof IClasspathEntry) {
			return workspaceHelper.getRoot.getFolder(parentElement.path).members
		}
		return null;
	}

	override getElements(Object inputElement) {
		return null
	}

	private def boolean isRelevantClasspathEntry(IClasspathEntry entry) {
		return entry !== null //
		&& entry.entryKind == IClasspathEntry.CPE_SOURCE //
		&& !entry.path.segments.exists[matches('(src|xtend)-gen')]
	}

	override getParent(Object element) {
		if (element instanceof IResource) {
			val classpathEntry = getParentClasspathEntry(element)
			if (classpathEntry.isRelevantClasspathEntry) {
				return classpathEntry
			}
		}
		if (element instanceof IClasspathEntry) {
			return workspaceHelper.root.getFolder(element.path).project
		}
		return null
	}

	/**
	 * returns the parent folder as ClasspathEntry or null if it is not a ClasspathEntry.
	 */
	def private IClasspathEntry getParentClasspathEntry(IResource element) {
		if (element.project != null) {
			val parentClasspathEntry = getChildren(element.project).filter(IClasspathEntry).filter [
				isRelevantClasspathEntry
			].findFirst [
				workspaceHelper.root.getFolder(path).members.contains(element)
			]
			return parentClasspathEntry
		}
		return null
	}

	override hasChildren(Object element) {
		if (element instanceof IClasspathEntry) {
			return workspaceHelper.root.getFolder(element.path).members.length > 0
		}
		return false
	}

	override dispose() {
		// nothing to do
	}

	override inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		// nothing to do
	}

}
