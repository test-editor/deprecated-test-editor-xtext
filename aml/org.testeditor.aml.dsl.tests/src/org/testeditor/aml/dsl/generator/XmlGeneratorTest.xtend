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
package org.testeditor.aml.dsl.generator

import javax.inject.Inject
import org.junit.Test

/** 
 * Tests for {@link XmlGenerator} 
 */
class XmlGeneratorTest extends AbstractGeneratorTest {
	
	@Inject XmlGenerator generator
	
	@Test
	def void testGetPackageFolder() {
		// Given
		val model = factory.createAmlModel => [
			package = "com.example"
		]
		
		// When
		val packageFolder = generator.getPackageFolder(model)
		
		// Then
		packageFolder.assertEquals("com/example/")
	}
	
}