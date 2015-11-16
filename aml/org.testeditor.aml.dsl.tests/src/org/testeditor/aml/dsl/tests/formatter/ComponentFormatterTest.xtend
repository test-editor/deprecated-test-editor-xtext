/*******************************************************************************
 * Copyright (c) 2012 - 2015 Signal Iduna Corporation and others.
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
package org.testeditor.aml.dsl.tests.formatter

import org.junit.Ignore
import org.junit.Test
import org.testeditor.aml.model.Component

/**
 * Formatting tests specific to {@link Component}.
 */
class ComponentFormatterTest extends AbstractFormatterTest {
	
	@Test @Ignore
	def void formatInheritance() {
		assertFormatted[
			expectation = '''
				component MyWizard is Dialog includes DefaultDialog
				
				abstract component DefaultDialog is Dialog
				
				component type Tree
				component type Composite
			'''
			toBeFormatted = '''
				component MyWizard   is Dialog	includes 	DefaultDialog
				  abstract	component  DefaultDialog is Dialog
				component type Dialog
			'''
		]
	}
	
}