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
package org.testeditor.tml.dsl.validation

import com.google.inject.Provider
import javax.inject.Inject
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.resource.XtextResourceSet
import org.junit.Before
import org.junit.Test
import org.testeditor.aml.AmlModel
import org.testeditor.aml.dsl.AmlStandaloneSetup
import org.testeditor.tml.dsl.tests.parser.AbstractParserTest

class TmlParameterUsageValidatorTest extends AbstractParserTest {
	@Inject TmlValidator tmlValidator // class under test
	@Inject protected Provider<XtextResourceSet> resourceSetProvider
	@Inject protected XtextResourceSet resourceSet

	protected ParseHelper<AmlModel> amlParseHelper

	@Before
	def void setUp() {
		resourceSet = resourceSetProvider.get
		resourceSet.classpathURIContext = this
		val injector = (new AmlStandaloneSetup).createInjectorAndDoEMFRegistration
		amlParseHelper = injector.getInstance(ParseHelper)
		amlParseHelper.parse('''
			package org.test

			import org.testeditor.dsl.common.testing.*

			interaction type start {
				template = "start" ${appname}
				method = DummyFixture.startApplication(appname)
			}

			interaction type wait {
				template = "wait" ${secs}
				method = DummyFixture.waitSeconds(secs)
			}

			interaction type unknown {
				template = "unknown" ${usage}
			}

			component type DummyCT {
				interactions = start, wait
			}

			component Dummy is DummyCT {}
		''', resourceSet)

	}

	@Test
	def void testDirectCallVariableTypeChecks() {
		// given
		val tmlModel = parser.parse('''
			package org.test

			import org.test.*

			# MacroCollection

			template = "mycall" ${appname}
			Component: Dummy
			- start @appname

			template = "othercall" ${secs}
			Component: Dummy
			- wait @secs
		''', resourceSet)

		val myCallContext = tmlModel.macros.head.contexts.head
		val otherCallContext = tmlModel.macros.last.contexts.head

		// when
		val setWithString = tmlValidator.getTypeUsagesOfVariable(myCallContext, "appname")
		val setWithLong = tmlValidator.getTypeUsagesOfVariable(otherCallContext, "secs")

		// then
		setWithString.assertSize(1)
		setWithString.head.simpleName.assertEquals(String.simpleName)

		setWithLong.assertSize(1)
		setWithLong.head.simpleName.assertEquals(long.simpleName)
	}

	@Test
	def void testIndirectCallVariableTypeChecks() {
		// given
		val tmlModel = parser.parse('''
			package org.test

			import org.test.*

			# MacroCollection

			template = "mycall" ${unknown}
			Macro: MacroCollection
			- othercall @unknown

			template = "othercall" ${secs}
			Component: Dummy
			- wait @secs
		''', resourceSet)

		val myCallContext = tmlModel.macros.head.contexts.head

		// when
		val setWithLong = tmlValidator.getTypeUsagesOfVariable(myCallContext, "unknown")

		// then
		setWithLong.assertSize(1)
		setWithLong.head.simpleName.assertEquals(long.simpleName)
	}

	@Test
	def void testIndirectCallVariableTypeChecksWithUnknowns() {
		// given
		val tmlModel = parser.parse('''
			package org.test

			import org.test.*

			# MacroCollection

			template = "mycall" ${unknown}
			Macro: MacroCollection
			- othercall @unknown

			template = "othercall" ${secs}
			Component: Dummy
			- wait @secs
			- unknown @secs
		''', resourceSet)

		val myCallContext = tmlModel.macros.head.contexts.head

		// when
		val setWithLong = tmlValidator.getTypeUsagesOfVariable(myCallContext, "unknown")

		// then
		setWithLong.assertSize(1)
		setWithLong.head.simpleName.assertEquals(long.simpleName)
	}
}
