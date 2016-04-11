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
package org.testeditor.rcp4.views.teststepselector

import javax.inject.Inject
import org.junit.Test
import org.testeditor.aml.dsl.tests.AbstractTest
import org.testeditor.aml.impl.AmlFactoryImpl

class AmlModelTreeDropTextProviderTest extends AbstractTest {

	@Inject AmlModelTreeDropTextProvider dropTextProvider
	@Inject AmlFactoryImpl amlFactory

	@Test
	def void elementReplacementRestrictedToUiElementReferences() {
		// when
		val result = dropTextProvider.adjustTextBy(0, amlFactory.createComponentElement => [name = "Test"],
			"<MyTest> <element> element <noelement> <lement>")

		// then
		assertEquals(result, "<MyTest> <Test> element <noelement> <lement>")
	}

}
