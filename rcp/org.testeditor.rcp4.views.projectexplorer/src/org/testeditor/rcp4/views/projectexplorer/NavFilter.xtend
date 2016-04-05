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

import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IFolder
import org.eclipse.jdt.core.IClasspathEntry
import org.eclipse.jdt.core.ICompilationUnit
import org.eclipse.jdt.internal.core.JarPackageFragmentRoot
import org.eclipse.jdt.internal.core.JavaElement
import org.eclipse.jface.viewers.Viewer
import org.eclipse.jface.viewers.ViewerFilter

/** 
 * Filter all classpath containers (Java RT, JUnit) and java elements from navigator
 * 
 *  thus the user of the rcp sees only the files relevant to him in the project explorer
 */
class NavFilter extends ViewerFilter {
	override select(Viewer viewer, Object parentElement, Object element) {
		if (element instanceof ICompilationUnit) { // hide all java compilation units
			return false
		}
		if (element instanceof IClasspathEntry) {
			if (element.path.toString.endsWith("-gen")) {
				return false
			}
		}
		if (element instanceof IFolder) { // don't show any folders that hold generated artifacts (-gen) or maven artifacts (target)
			if (element.projectRelativePath.segments.exists [
				equals("target") || equals("build") || equals("gradle") || equals("bin") || endsWith("-gen")
			]) {
				return false;
			}
		}
		if (element instanceof IFile) {
			if (element.projectRelativePath.lastSegment.matches("pom.xml|gradle.+|.+gradle")) { // don't show maven pom.xml
				return false
			}
			if (element.projectRelativePath.fileExtension?.matches("java|xtend|groovy")) { // don't show java nor xtend files
				return false
			}
		}
		if (element instanceof JarPackageFragmentRoot) { // don't show any included jars (e.g. junit.jar)
			return false
		}
		if (element instanceof JavaElement) { // don't show any java project relevant files of generated artifacts (-gen) 
			val segments = element.getResource.projectRelativePath.segments
			return !(segments.exists[endsWith("-gen")])
		}
		return true
	}
}
