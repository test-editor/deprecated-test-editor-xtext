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
import org.eclipse.core.runtime.Path
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder
import org.mockito.InjectMocks
import org.testeditor.dsl.common.testing.AbstractTest

import static org.junit.Assume.*

class GradleClasspathUtilTest extends AbstractTest {

	@InjectMocks
	GradleClasspathUtil classpathUtil
	@Rule public TemporaryFolder tempFolder = new TemporaryFolder();

	@Test
	def void testGetGradleClasspathEntries() {
		assumeTrue(new GradleServerConnectUtil().canConnet())
		// given
		val propertyOutput = getGradlePropertiesPrintOut(tempFolder.root)

		// when
		val result = classpathUtil.parseGradleProperties(propertyOutput)

		// then
		assertTrue(result.contains(new Path(tempFolder.root + "/src/main/java")))
		assertTrue(result.contains(new Path(tempFolder.root + "/src/test/java")))
		assertTrue(result.contains(new Path(tempFolder.root + "/src/integration test/java")))
	}

	def String getGradlePropertiesPrintOut(File prjDir) {
		'''
			processIntegrationTestResources: task ':processIntegrationTestResources'
			processResources: task ':processResources'
			processTestResources: task ':processTestResources'
			projectEvaluationBroadcaster: ProjectEvaluationListener broadcast
			projectEvaluator: org.gradle.configuration.project.LifecycleProjectEvaluator@e640c9b
			projectDir: «prjDir»
			projectRegistry: org.gradle.api.internal.project.DefaultProjectRegistry@33f7ff15
			reporting: org.gradle.api.reporting.ReportingExtension_Decorated@612f4607
			resources: org.gradle.api.internal.resources.DefaultResourceHandler@520fa483
			scriptHandlerFactory: org.gradle.api.internal.initialization.DefaultScriptHandlerFactory@28a2fb34
			scriptPluginFactory: org.gradle.configuration.DefaultScriptPluginFactory@11afad47
			serviceRegistryFactory: org.gradle.internal.service.scopes.ProjectScopeServices$4@3e2b284d
			services: ProjectScopeServices
			sourceCompatibility: 1.8
			sourceSets: [source set 'integration test', source set 'main', source set 'test']
			standardOutputCapture: org.gradle.logging.internal.DefaultLoggingManager@10759d2c
			state: project state 'EXECUTED'
			status: integration
			subprojects: []
			targetCompatibility: 1.8
			tasks: [task ':assemble', task ':buildDependents', task ':buildNeeded', task ':check', task ':classes', task ':cleanEclipse', task ':cleanEclipseClasspath', task ':cleanEclipseJdt', task ':cleanEclipseJdtUi', task ':cleanEclipseProject', task ':compileIntegrationTestJava', task ':compileJava', task ':compileTestJava', task ':createRelease', task ':currentVersion', task ':deleteGlobalLock', task ':deleteLock', task ':dependencyManagement', task ':eclipse', task ':eclipseClasspath', task ':eclipseJdt', task ':eclipseJdtCoreSettings', task ':eclipseJdtUiSettings', task ':eclipseProject', task ':eclipseSettings', task ':flywayBaseline', task ':flywayClean', task ':flywayInfo', task ':flywayMigrate', task ':flywayRepair', task ':flywayValidate', task ':generateGlobalLock', task ':generateLock', task ':glassfishEmbeddedTest', task ':install', task ':integrationTest', task ':integrationTestClasses', task ':jacocoTestReport', task ':jar', task ':javadoc', task ':markNextVersion', task ':processIntegrationTestResources', task ':processResources', task ':processTestResources', task ':properties', task ':pushRelease', task ':release']
			test: task ':test'
			testClasses: task ':testClasses'
		'''
	}

}
