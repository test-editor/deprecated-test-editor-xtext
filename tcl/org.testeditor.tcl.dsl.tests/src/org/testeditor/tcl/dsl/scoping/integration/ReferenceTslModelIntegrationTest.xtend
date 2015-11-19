package org.testeditor.tcl.dsl.scoping.integration

import javax.inject.Inject
import javax.inject.Provider
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.resource.XtextResourceSet
import org.junit.Before
import org.junit.Test
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest
import org.testeditor.tsl.TslModel
import org.testeditor.tsl.dsl.TslStandaloneSetup

import static extension org.eclipse.emf.common.util.URI.createFileURI

class ReferenceTslModelIntegrationTest extends AbstractParserTest {

	@Inject Provider<XtextResourceSet> resourceSetProvider

	XtextResourceSet resourceSet
	ParseHelper<TslModel> tslParser

	@Before
	def void setup() {
		resourceSet = resourceSetProvider.get
		val tslInjector = (new TslStandaloneSetup).createInjectorAndDoEMFRegistration
		tslParser = tslInjector.getInstance(ParseHelper)

	}

	private def parseTslModel(String packageName) {
		val tsl = '''
			package «packageName»
			
			* First step
		'''
		val tslModel = tslParser.parse(tsl, 'DummySpec.tsl'.createFileURI, resourceSet)
		tslModel.assertNoErrors
		return tslModel
	}

	@Test
	def void canReferenceTslModelInSamePackage() {
		// given
		val tslModel = parseTslModel('com.example')
		val tcl = '''
			package com.example
			
			implements DummySpec
		'''

		// when
		val tclModel = parser.parse(tcl, resourceSet)

		// then
		tclModel.assertNoErrors
		tclModel.specification.assertSame(tslModel)
	}

	@Test
	def void canImportTslModel() {
		// given
		val tslModel = parseTslModel('some.other')
		val tcl = '''
			package com.example
			
			import some.other.*
			
			implements DummySpec
		'''

		// when
		val tclModel = parser.parse(tcl, resourceSet)

		// then
		tclModel.assertNoErrors
		tclModel.specification.assertSame(tslModel)
	}

	@Test
	def void referenceSpecificationStep() {
		// given
		val tslModel = parseTslModel('com.example')
		val firstStep = tslModel.steps.assertSingleElement
		val tcl = '''
			package com.example
			
			implements DummySpec
			
			* First step
		'''
		val tclModel = parser.parse(tcl, resourceSet)
		tclModel.assertNoErrors
		val firstStepImpl = tclModel.steps.assertSingleElement

		// when
		val specification = firstStepImpl.specification

		// then
		specification.assertSame(firstStep)
	}

}