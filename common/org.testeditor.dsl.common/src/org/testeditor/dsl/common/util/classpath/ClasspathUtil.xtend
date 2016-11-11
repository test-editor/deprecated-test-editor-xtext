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
package org.testeditor.dsl.common.util.classpath

import javax.inject.Inject
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.eclipse.emf.ecore.EObject
import org.eclipse.jdt.core.IClasspathEntry
import org.eclipse.jdt.core.JavaCore
import org.eclipse.xtext.EcoreUtil2
import org.slf4j.LoggerFactory
import org.testeditor.dsl.common.util.WorkspaceHelper

class ClasspathUtil {

	static val logger = LoggerFactory.getLogger(ClasspathUtil)

	@Inject WorkspaceHelper workspaceHelper
	@Inject MavenClasspathUtil mavenClasspathUtil
	@Inject GradleClasspathUtil gradleClasspathUtil

	def String inferPackage(EObject element) {
		val orignPath = new Path(EcoreUtil2.getPlatformResourceOrNormalizedURI(element).trimFragment.path)
		var path = orignPath.removeLastSegments(1)
		var IPath cpEntry = new Path("")
		if (orignPath.isEclipseResolved) {
			path = orignPath.removeFirstSegments(1).removeLastSegments(1)
			cpEntry = path.getEclipseClasspathEntry
		} else {
			path = new Path(path.toFile.absolutePath)
			cpEntry = path.getBuildToolClasspathEntry
		}
		val start = path.matchingFirstSegments(cpEntry)
		val result = path.removeFirstSegments(start).segments.join(".")
		logger.debug("Inferred package for {} is {}.", element, result)
		return result
	}

	def IPath getBuildToolClasspathEntry(IPath path) {
		logger.info("Searching classpath for {}.", path)
		val baseDir = path.getBuildProjectBaseDir
		if (baseDir.toFile.list.contains("pom.xml")) {
			return mavenClasspathUtil.getMavenClasspathEntries(baseDir).filter[it.isPrefixOf(path)].head
		}
		if (baseDir.toFile.list.contains("build.gradle")) {
			return gradleClasspathUtil.getGradleClasspathEntries(baseDir).filter[it.isPrefixOf(path)].head
		}
		return null
	}

	def protected IPath getEclipseClasspathEntry(IPath path) {
		val javaProject = JavaCore.create(workspaceHelper.root.getFile(path).project)
		val classpathEntries = javaProject.rawClasspath.filter[entryKind == IClasspathEntry.CPE_SOURCE]
		return classpathEntries.filter[it.path.isPrefixOf(path)].head.path
	}

	def protected boolean getIsEclipseResolved(Path path) {
		return path.toString.startsWith("/resource/")
	}

	def IPath getBuildProjectBaseDir(IPath path) {
		if (path.toFile.list.contains("pom.xml") || path.toFile.list.contains("build.gradle")) {
			return path
		}
		if (path.toFile.parent != null) {
			return getBuildProjectBaseDir(new Path(path.toFile.parent))
		}
		return null
	}

}
