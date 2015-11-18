package org.testeditor.tcl.dsl.jvmmodel

import org.junit.Test

import static extension org.eclipse.emf.common.util.URI.createFileURI

class SimpleTclGeneratorIntegrationTest extends AbstractTclGeneratorIntegrationTest {

	@Test
	def void test() {
		// given
		val aml = '''
			package com.example
			
			component type Application
			component GreetingApplication is Application
		'''
		val tcl = '''
			package com.example
			
			* Start the famous greetings application
				Mask: GreetingApplication
				- starte Anwendung "org.testeditor.swing.exammple.Greetings"
				- stoppe Anwendung.
		'''
		amlParseHelper.parse(aml, resourceSet)
		val tclModel = tclParseHelper.parse(tcl, 'SimpleTest.tcl'.createFileURI, resourceSet)

		// when
		val obj = generate(tclModel)

		// then
		println(obj)
	}

}