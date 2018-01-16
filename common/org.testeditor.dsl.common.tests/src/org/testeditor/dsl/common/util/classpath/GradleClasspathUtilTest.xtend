/*******************************************************************************
 * Copyright (c) 2012 - 2018 Signal Iduna Corporation and others.
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
import org.eclipse.core.runtime.Path
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder
import org.mockito.InjectMocks
import org.testeditor.dsl.common.testing.AbstractTest

class GradleClasspathUtilTest extends AbstractTest {

	@InjectMocks
	GradleClasspathUtil classpathUtil
	@Rule public TemporaryFolder tempFolder = new TemporaryFolder();

	@Test
	def void testGetGradleClasspathEntries() {
		// given
		val sourceSetPathsOutput = getGradleSourceSetPathsPrintOut(tempFolder.root)

		// when
		val result = classpathUtil.parseGradleSourcePaths(sourceSetPathsOutput)

		// then
		assertTrue(result.contains(new Path(tempFolder.root + "/src/main/java")))
		assertTrue(result.contains(new Path(tempFolder.root + "/src/test/java")))
		assertTrue(result.contains(new Path(tempFolder.root + "/src/integration test/java")))
	}

	def String getGradleSourceSetPathsPrintOut(File prjDir) {
		'''
			Picked up _JAVA_OPTIONS: -Djdk.http.auth.tunneling.disabledSchemes=
			:sourceSetPaths
			sourceSetPath: '«prjDir»/src/main/java'
			sourceSetPath: '«prjDir»/build/tcl/main'
			sourceSetPath: '«prjDir»/build/tclConfig/main'
			sourceSetPath: '«prjDir»/build/tclMacro/main'
			sourceSetPath: '«prjDir»/src/test/java'
			sourceSetPath: '«prjDir»/build/tcl/test'
			sourceSetPath: '«prjDir»/build/tclConfig/test'
			sourceSetPath: '«prjDir»/build/tclMacro/test'
			sourceSetPath: '«prjDir»/src/integration test/java'
			
			BUILD SUCCESSFUL
			
			Total time: 0.707 secs
		'''
	}

}
