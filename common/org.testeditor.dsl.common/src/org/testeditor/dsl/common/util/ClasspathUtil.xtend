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
package org.testeditor.dsl.common.util

import java.io.File
import java.io.FileInputStream
import java.io.InputStream
import java.util.ArrayList
import java.util.List
import javax.inject.Inject
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.xpath.XPathConstants
import javax.xml.xpath.XPathFactory
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.core.runtime.Path
import org.eclipse.emf.ecore.EObject
import org.eclipse.jdt.core.IClasspathEntry
import org.eclipse.jdt.core.JavaCore
import org.eclipse.xtext.EcoreUtil2
import org.slf4j.LoggerFactory
import org.w3c.dom.Document
import org.w3c.dom.Node

class ClasspathUtil {

	static val EFFECTIVE_POM_TXT_PATH = "target/effective_pom.txt"
	static val logger = LoggerFactory.getLogger(ClasspathUtil)

	@Inject WorkspaceRootHelper workspaceRootHelper
	@Inject MavenExecutor mavenCommand
	@Inject GradleCommand gradleCommand

	List<IPath> mavenClasspath
	List<IPath> gradleClasspath

	def String inferPackage(EObject element) {
		val orignPath = new Path(EcoreUtil2.getPlatformResourceOrNormalizedURI(element).trimFragment.path)
		var path = orignPath.removeLastSegments(2)
		var IPath cpEntry = new Path("")
		if (orignPath.isEclipseResolved) {
			path = orignPath.removeFirstSegments(1).removeLastSegments(2)
			cpEntry = path.getEclipseClasspathEntry
		} else {
			path = new Path(path.toFile.absolutePath)
			cpEntry = path.getBuildToolClasspathEntry
		}
		val start = path.matchingFirstSegments(cpEntry)
		return path.removeFirstSegments(start).segments.join(".")
	}

	def IPath getBuildToolClasspathEntry(IPath path) {
		logger.info("Searching classpath for {}.", path)
		val baseDir = path.getBuildProjectBaseDir
		if (baseDir.toFile.list.contains("pom.xml")) {
			return baseDir.getMavenClasspathEntries().filter[it.isPrefixOf(path)].head
		}
		if (baseDir.toFile.list.contains("build.gradle")) {
			return baseDir.getGradleClasspathEntries().filter[it.isPrefixOf(path)].head
		}
		return null
	}

	def List<IPath> getGradleClasspathEntries(IPath path) {
		val result = new ArrayList<IPath>()
		if (gradleClasspath == null) {
			val output = gradleCommand.execute(path.toFile, "properties")
			val props = output.split(System.getProperty("line.separator"))
			val prjDir = props.filter[startsWith("projectDir")].head.split(": ").get(1)
			val sourceSetProperty = props.filter[startsWith("sourceSets")].head
			val sourceSets = sourceSetProperty.substring(sourceSetProperty.indexOf("[") + 1,
				sourceSetProperty.lastIndexOf("]")).split(",")
			val javaSourceSet = sourceSets.filter[it.trim.startsWith("source set")]
			javaSourceSet.forEach [
				result.add(
					new Path(prjDir + "/src/" + it.substring(it.indexOf("'") + 1, it.lastIndexOf("'")) + "/java"))
			]
		}
		return result
	}

	/**
	 * Read ones per jvm run the maven classpath. every other call run the first selected information. This is intended for batch runs.
	 */
	def List<IPath> getMavenClasspathEntries(IPath path) {
		if (mavenClasspath == null) {
			mavenCommand.executeInNewJvm("help:effective-pom", path.toOSString,
				"output=" + path.toOSString + "/" + EFFECTIVE_POM_TXT_PATH, new NullProgressMonitor(), System.out, true)
			val effectivePom = new File(path.toFile, EFFECTIVE_POM_TXT_PATH)
			if (effectivePom.exists) {
				mavenClasspath = readMavenClasspathEntriesFromPom(new FileInputStream(effectivePom))
			} else {
				logger.warn(
					"Could not find the genrated effective pom in {}. This is needed to look up the build path ",
					effectivePom)
				}
				return mavenClasspath
			}
		}

		def List<IPath> readMavenClasspathEntriesFromPom(InputStream pomStream) {
			val doc = DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(pomStream)
			val node = doc.getElementsByTagName("build").item(0)
			val mavenPaths = #["sourceDirectory", "testSourceDirectory", "scriptSourceDirectory"]
			val childs = node.childNodes
			val result = new ArrayList<IPath>()
			for (var i = 0; i < childs.length; i++) {
				if (mavenPaths.contains(childs.item(i).nodeName)) {
					result.add(new Path(childs.item(i).textContent))
				}
			}
			result.addAll(searchForBuildHelperPluginDirectories(doc))
			return result
		}

		def List<IPath> searchForBuildHelperPluginDirectories(Document document) {
			val result = new ArrayList<IPath>()
			val xpath = XPathFactory.newInstance().newXPath();
			val expression = "/project/build/plugins/plugin[artifactId='build-helper-maven-plugin']";
			val helperPluginNode = xpath.evaluate(expression, document, XPathConstants.NODE) as Node;
			if (helperPluginNode != null) {
				val sources = xpath.evaluate("executions/execution/configuration/sources", helperPluginNode,
					XPathConstants.NODE) as Node;
				val sourcesChilds = sources.childNodes
				for (var i = 0; i < sourcesChilds.length; i++) {
					if (sourcesChilds.item(i).nodeName.equals("source")) {
						result.add(new Path(sourcesChilds.item(i).textContent))
					}
				}
			}
			return result
		}

		def IPath getEclipseClasspathEntry(IPath path) {
			val javaProject = JavaCore.create(workspaceRootHelper.root.getFile(path).project)
			val classpathEntries = javaProject.rawClasspath.filter[entryKind == IClasspathEntry.CPE_SOURCE]
			return classpathEntries.filter[it.path.isPrefixOf(path)].head.path
		}

		def boolean getIsEclipseResolved(Path path) {
			return path.toString.startsWith("/resource/")
		}

		def IPath getBuildProjectBaseDir(IPath path) {
			println(">>>>>>>>>>>>> " + path)
			if (path.toFile.list.contains("pom.xml") || path.toFile.list.contains("build.gradle")) {
				return path
			}
			if (path.toFile.parent != null) {
				return getBuildProjectBaseDir(new Path(path.toFile.parent))
			}
			return null
		}

	}
	