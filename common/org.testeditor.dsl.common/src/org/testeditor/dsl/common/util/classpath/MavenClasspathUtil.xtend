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

import java.io.File
import java.io.FileInputStream
import java.io.InputStream
import java.util.ArrayList
import java.util.List
import com.google.inject.Inject
import javax.inject.Singleton
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.xpath.XPathConstants
import javax.xml.xpath.XPathFactory
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.core.runtime.Path
import org.slf4j.LoggerFactory
import org.testeditor.dsl.common.util.MavenExecutor
import org.w3c.dom.Document
import org.w3c.dom.Node

@Singleton
class MavenClasspathUtil {

	static val EFFECTIVE_POM_TXT_PATH = "target/effective_pom.txt"
	static val logger = LoggerFactory.getLogger(MavenClasspathUtil)

	@Inject MavenExecutor mavenCommand

	List<IPath> mavenClasspath

	/**
	 * Read ones per jvm run the maven classpath. every other call run the first selected information. This is intended for batch runs.
	 */
	def List<IPath> getMavenClasspathEntries(IPath path) {
		if (mavenClasspath == null) {
			mavenCommand.executeInNewJvm("help:effective-pom", path.toOSString,
				"output=" + path.append(EFFECTIVE_POM_TXT_PATH).toOSString, new NullProgressMonitor(), System.out)
			val effectivePom = new File(path.toFile, EFFECTIVE_POM_TXT_PATH)
			if (effectivePom.exists) {
				mavenClasspath = readMavenClasspathEntriesFromPom(new FileInputStream(effectivePom))
			} else {
				logger.warn("Could not find the generated effective pom in {}. This is used to determine build path.",
					effectivePom)
			}
		}
		return mavenClasspath
	}

	def protected List<IPath> readMavenClasspathEntriesFromPom(InputStream pomStream) {
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

	def protected List<IPath> searchForBuildHelperPluginDirectories(Document document) {
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

}
