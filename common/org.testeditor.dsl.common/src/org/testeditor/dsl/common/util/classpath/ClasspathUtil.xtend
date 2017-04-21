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
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.NullProgressMonitor
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
		if (cpEntry === null) {
			logger.error("Could not find corresponding classpath entry for path='{}'.", path)
			return 'com.example'
		}
		val start = path.matchingFirstSegments(cpEntry)
		val result = path.removeFirstSegments(start).segments.join(".")
		logger.debug("Inferred package for {} is {}.", element, result)
		return result
	}
	
	def boolean isGradleProjectBaseDir(IPath baseDir) {
		return baseDir.toFile.list.contains("build.gradle")
	}
	
	def boolean isMavenProjectBaseDir(IPath baseDir) {
		return baseDir.toFile.list.contains("pom.xml")
	}

	def IPath getBuildToolClasspathEntry(IPath path) {
		logger.info("Searching classpath for {}.", path)
		val baseDir = path.getBuildProjectBaseDir
		switch (baseDir) {
			case baseDir.isMavenProjectBaseDir: return mavenClasspathUtil.getMavenClasspathEntries(baseDir).findFirst[
				logger.debug("Checking maven classpath='{}'.", it.toOSString)
				it.isPrefixOf(path)
			]
			case baseDir.isGradleProjectBaseDir: return gradleClasspathUtil.getGradleSourceSetPaths(baseDir).findFirst[
				logger.debug("Checking gradle classpath='{}'.", it.toOSString)
				it.isPrefixOf(path)
			]
			default: throw new RuntimeException('''Unknown project type in project base dir='«baseDir»'.''')
		}
	}

	def protected IPath getEclipseClasspathEntry(IPath path) {
		val classpathEntries = getSourceClasspathEntries(workspaceHelper.root.getFile(path).project)
		return classpathEntries.filter[it.path.isPrefixOf(path)].head.path
	}

	def Iterable<IClasspathEntry> getSourceClasspathEntries(IProject project) {
		if (project.hasJavaNature) {
			val javaProject = JavaCore.create(project)
			return javaProject.rawClasspath.filter[entryKind == IClasspathEntry.CPE_SOURCE]
		} else {
			return emptyList
		}
	}

	/**
	 * add a list of additional class path entries to this project (if it has the java nature)
	 */
	def void addClasspathEntries(IProject project, Iterable<IClasspathEntry> entriesToAdd) {
		if (project.hasJavaNature) {
			val javaProject = JavaCore.create(project)
			val existingClasspath = javaProject.rawClasspath
			val newClasspathEntries = newArrayOfSize(existingClasspath.length + entriesToAdd.size)
			val result = (entriesToAdd + existingClasspath).toList.toArray(newClasspathEntries)
			javaProject.setRawClasspath(result, NullProgressMonitor.newInstance)
		}
	}

	/** 
	 * get all classpath entries of this java-project, apply the transformation 
	 * and set the classpaths of this project to the transformed classpath entries
	 * removing nulls if exsitent
	 */
	def void transformClasspathEntries(IProject project, (IClasspathEntry)=>IClasspathEntry transformation) {
		if (project.hasJavaNature) {
			val javaProject = JavaCore.create(project)
			val transformedClasspaths = javaProject.rawClasspath.map(transformation).filterNull
			javaProject.setRawClasspath(transformedClasspaths, new NullProgressMonitor)
		} else {
			throw new IllegalArgumentException('Passed project must have the java project nature.')
		}
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

	private def boolean hasJavaNature(IProject project) {
		try {
			return project.hasNature(JavaCore.NATURE_ID)
		} catch (CoreException e) {
			return false
		}
	}

}
