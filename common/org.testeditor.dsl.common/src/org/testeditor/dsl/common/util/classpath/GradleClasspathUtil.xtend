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

import java.util.ArrayList
import java.util.List
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import javax.inject.Inject
import org.testeditor.dsl.common.util.GradleCommand

class GradleClasspathUtil {

	@Inject GradleCommand gradleCommand

	List<IPath> gradleClasspath

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

}
