package org.testeditor.tcl.dsl.jvmmodel

import com.google.inject.Inject
import com.google.inject.Provider
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.resource.XtextResourceSet
import org.junit.Before
import org.junit.Test
import org.testeditor.aml.dsl.AmlStandaloneSetup
import org.testeditor.tcl.dsl.tests.AbstractTest

class TclJvmModelInferrerIntegrationTest extends AbstractTest {
	
	@Inject
	Provider<XtextResourceSet> resourceSetProvider
	
	@Inject
	XtextResourceSet resourceSet
	
	@Before
	def void parseAmlModel() {
		resourceSet = resourceSetProvider.get
		
		val aml = '''
			package com.example
			
			component type Application
		'''
		
		val injector = (new AmlStandaloneSetup).createInjectorAndDoEMFRegistration
		val parseHelper = injector.getInstance(ParseHelper)
		
		parseHelper.parse(aml, resourceSet)
		
	}
	
	@Test
	def void test() {
		println(resourceSet.resources)
	}
	
}