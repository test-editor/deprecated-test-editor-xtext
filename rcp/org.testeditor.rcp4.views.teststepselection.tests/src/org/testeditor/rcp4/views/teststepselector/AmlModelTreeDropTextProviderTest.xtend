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
package org.testeditor.rcp4.views.teststepselector

import javax.inject.Inject
import org.junit.Test
import org.testeditor.aml.AmlFactory
import org.testeditor.aml.dsl.tests.AbstractAmlTest

class AmlModelTreeDropTextProviderTest extends AbstractAmlTest {

	static val aml = AmlFactory.eINSTANCE
	@Inject AmlModelTreeDropTextProvider dropTextProvider

	@Test
	def void elementReplacementRestrictedToUiElementReferences() {
		// given
		val componentElement = aml.createComponentElement => [
			name = "Test"
		]
		val replacementCandidates = "<MyTest> <element> element <noelement> <lement>"

		// when
		val result = dropTextProvider.adjustTextBy(0, componentElement, replacementCandidates)

		// then
		result.assertEquals("<MyTest> <Test> element <noelement> <lement>")
	}

}
