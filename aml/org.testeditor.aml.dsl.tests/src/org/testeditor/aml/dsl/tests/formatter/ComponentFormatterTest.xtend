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
package org.testeditor.aml.dsl.tests.formatter

import org.junit.Test
import org.testeditor.aml.Component

/**
 * Formatting tests specific to {@link Component}.
 */
class ComponentFormatterTest extends AbstractFormatterTest {
	
	@Test
	def void formatInheritance() {
		assertFormatted[
			expectation = '''
				component MyWizard is Dialog includes DefaultDialog
				
				abstract component DefaultDialog is Dialog
				
				component type Dialog
				
				component type Composite
			'''
			toBeFormatted = '''
				component MyWizard   is Dialog	includes 	DefaultDialog
				  abstract	component  DefaultDialog is Dialog
				component type Dialog component type Composite
			'''
		]
	}
	
	@Test
	def void formatComponent() {
		// if there is a single line between elements leave as it is
		assertFormatted[
			expectation = '''
				component LoginPage is WebPage {
				
					element User is Text {

					}
				
				}
			'''
			toBeFormatted = expectation
		]
		// if the user chooses to not put in empty lines, leave as it is
		assertFormatted[
			expectation = '''
				component LoginPage is WebPage {
					element User is Text {
					}
				}
			'''
			toBeFormatted = expectation
		]
		// if there is more than one empty line, format as just one empty line in-between
		assertFormatted[
			expectation = '''
				component LoginPage is WebPage {
				
					element User is Text {

					}
				
				}
			'''
			toBeFormatted = '''
				component LoginPage is WebPage {
					
					
					element User is Text {
						
						
					}
					
					
				}
			'''
		]
	}
	
}
