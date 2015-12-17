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

import org.junit.Test

class InteractionTypeFormatterTest extends AbstractFormatterTest {

	@Test
	def void regressionInteractionType() {
		assertFormatted[
			expectation = '''
				interaction type Pruefe_Text_Nicht_In_Element {
					label = "Ist Text am Element nicht vorhanden"
					template = "überprüfe ob am Element" ${element} "der Text" ${} "nicht vorhanden ist"
				}
			'''

			toBeFormatted = '''
				interaction
				type Pruefe_Text_Nicht_In_Element {
					label = "Ist Text am Element nicht vorhanden" template = "überprüfe ob am Element" ${element} "der Text" ${} "nicht vorhanden ist"
				}
			'''
		]
	}

}