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
package org.testeditor.tcl.dsl.validation

import com.google.inject.Provider
import javax.inject.Inject
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.resource.XtextResourceSet
import org.junit.Before
import org.junit.Test
import org.testeditor.aml.AmlModel
import org.testeditor.aml.dsl.AmlStandaloneSetup
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest
import org.testeditor.tml.TmlModel
import org.testeditor.tml.dsl.TmlStandaloneSetup

import static org.testeditor.tml.TmlPackage.Literals.*

class TclParameterUsageValidatorTest extends AbstractParserTest {
	@Inject TclValidator tclValidator // class under test
	@Inject protected Provider<XtextResourceSet> resourceSetProvider
	@Inject protected XtextResourceSet resourceSet
	@Inject ValidationTestHelper validator

	protected ParseHelper<AmlModel> amlParseHelper
	protected ParseHelper<TmlModel> tmlParseHelper

	@Before
	def void setup() {
		resourceSet = resourceSetProvider.get
		resourceSet.classpathURIContext = this
		val injector = (new AmlStandaloneSetup).createInjectorAndDoEMFRegistration
		amlParseHelper = injector.getInstance(ParseHelper)
		val tmlInjector = (new TmlStandaloneSetup).createInjectorAndDoEMFRegistration
		tmlParseHelper = tmlInjector.getInstance(ParseHelper)

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

			component type DummyCT {
				interactions = start, wait
			}

			component Dummy is DummyCT {}
		''', resourceSet)

	}

	@Test
	def void testDirectCallVariableTypeChecks() {
		// given
		tmlParseHelper.parse('''
			package org.test

			# MacroCollection

			template = "mycall" ${appname}
			Component: Dummy
			- start @appname

			template = "othercall" ${secs}
			Component: Dummy
			- wait @secs
		''', resourceSet)

		val tclModel = parser.parse('''
			package org.test

			require envVar, myEnvString

			# MyTest

			* test something
			Macro: MacroCollection
			- mycall @myEnvString

			* test other
			Macro: MacroCollection
			- othercall @envVar
		''', resourceSet)

		val somethingContext = tclModel.test.steps.head.contexts.head
		val otherContext = tclModel.test.steps.last.contexts.head

		// when
		val setWithString = tclValidator.getTypeUsagesOfVariable(somethingContext, "myEnvString")
		val setWithLong = tclValidator.getTypeUsagesOfVariable(otherContext, "envVar")

		// then
		setWithString.assertSize(1)
		setWithString.head.simpleName.assertEquals(String.simpleName)

		setWithLong.assertSize(1)
		setWithLong.head.simpleName.assertEquals(long.simpleName)
	}

	@Test
	def void testIndirectCallVariableTypeChecks() {
		// given
		tmlParseHelper.parse('''
			package org.test

			# MacroCollection

			template = "mycall" ${unknown}
			Macro: MacroCollection
			- othercall @unknown

			template = "othercall" ${secs}
			Component: Dummy
			- wait @secs
		''', resourceSet)

		val tclModel = parser.parse('''
			package org.test

			require envVar, myEnvString

			# MyTest

			* test something
			Macro: MacroCollection
			- mycall @myEnvString
		''', resourceSet)

		val myCallContext = tclModel.test.steps.head.contexts.head

		// when
		val setWithLong = tclValidator.getTypeUsagesOfVariable(myCallContext, "myEnvString")

		// then
		setWithLong.assertSize(1)
		setWithLong.head.simpleName.assertEquals(long.simpleName)
	}

	@Test
	def void testIndirectCallVariableWithMultipleUsageTypeChecks() {
		// given
		tmlParseHelper.parse('''
			package org.test

			# MacroCollection

			template = "mycall" ${unknown}
			Macro: MacroCollection
			- othercall @unknown with @unknown

			template = "othercall" ${secs} "with" ${strParam}
			Component: Dummy
			- wait @secs
			- start @strParam
		''', resourceSet)

		val tclModel = parser.parse('''
			package org.test

			require envVar, myEnvString

			# MyTest

			* test something
			Macro: MacroCollection
			- mycall @myEnvString
		''', resourceSet)

		val myCallContext = tclModel.test.steps.head.contexts.head

		// when
		val setWithLong = tclValidator.getTypeUsagesOfVariable(myCallContext, "myEnvString")

		// then
		setWithLong.assertSize(2)
		setWithLong.map[simpleName].toList.contains(long.simpleName)
		setWithLong.map[simpleName].toList.contains(String.simpleName)
	}

	@Test
	def void testIndirectCallValidation() {
		// given + when
		tmlParseHelper.parse('''
			package org.test

			# MacroCollection

			template = "mycall" ${unknown}
			Macro: MacroCollection
			- othercall "3" with @unknown

			template = "othercall" ${secs} "with" ${strParam}
			Component: Dummy
			- wait @secs
			- start @strParam
		''', resourceSet)

		val tclModel = parser.parse('''
			package org.test

			require envVar, myEnvString

			# MyTest

			* test something
			Macro: MacroCollection
			- mycall @myEnvString
		''', resourceSet)

		// then
		validator.assertNoError(tclModel, TclValidator.INVALID_TYPED_VAR_DEREF)
	}

	@Test
	def void testIndirectCallValidationFails() {
		// given + when
		tmlParseHelper.parse('''
			package org.test

			# MacroCollection

			template = "mycall" ${unknown}
			Macro: MacroCollection
			- othercall @unknown with @unknown

			template = "othercall" ${secs} "with" ${strParam}
			Component: Dummy
			- wait @secs
			- start @strParam
		''', resourceSet)

		val tclModel = parser.parse('''
			package org.test

			require envVar, myEnvString

			# MyTest

			* test something
			Macro: MacroCollection
			- mycall @myEnvString
		''', resourceSet)

		// then
		validator.assertError(tclModel, TEST_STEP, TclValidator.INVALID_TYPED_VAR_DEREF)
	}
}
