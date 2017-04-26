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

import com.google.common.annotations.VisibleForTesting
import com.google.common.io.CharSource
import java.io.ByteArrayOutputStream
import java.util.List
import java.util.regex.Pattern
import javax.inject.Inject
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.slf4j.LoggerFactory
import org.testeditor.dsl.common.util.GradleHelper

class GradleClasspathUtil {

	static val logger = LoggerFactory.getLogger(GradleClasspathUtil)

	@Inject GradleHelper gradle

	List<IPath> gradleClasspath
	
	def void clearCache() {
		if (gradleClasspath !== null) {
			gradleClasspath.clear
			gradleClasspath = null
		}
	}

	def List<IPath> getGradleSourceSetPaths(IPath projectRoot) {
		if (gradleClasspath === null) {
			val outTask = new ByteArrayOutputStream()
			gradle.run(projectRoot.toFile, null, [
				withArguments("-q", "sourceSetPaths")
				standardOutput = outTask
				standardError = outTask
			])
			gradleClasspath = parseGradleSourcePaths(outTask.toString)
		}
		return gradleClasspath
	}

	/**
	 * try to extract source set paths from output: line format "sourceSet: 'PATH'"
	 */
	@VisibleForTesting
	protected def List<IPath> parseGradleSourcePaths(String output) {
		logger.info("Parsing gradle task output printProperties to extract source set.")
		val lines = CharSource.wrap(output).readLines
		val pattern = Pattern.compile("[^']*'([^']*)'")
		return lines.filter[startsWith('sourceSetPath:')] //
		.map[pattern.matcher(it)] //
		.filter[matches] //
		.map[new Path(group(1)) as IPath] //
		.toList
	}

}
