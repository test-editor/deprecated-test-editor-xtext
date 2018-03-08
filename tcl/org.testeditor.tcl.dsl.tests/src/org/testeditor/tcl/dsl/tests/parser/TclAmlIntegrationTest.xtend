package org.testeditor.tcl.dsl.tests.parser

import javax.inject.Inject
import org.junit.Test
import org.testeditor.dsl.common.testing.DummyFixture
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.util.TclModelUtil

class TclAmlIntegrationTest extends AbstractParserTest {

	@Inject extension TclModelUtil

	@Test
	def void testUnicodeInTemplates() {
		val aml = '''
			package com.examples
			
			import «DummyFixture.package.name».*
			
			interaction type withUnicodeTemplate {
				template = "Übersicht der Verträge" ${vertragsTüp} "λ"
				method = DummyFixture.startApplication(vertragsTüp)
			}
			
			component type MyType {
				interactions = withUnicodeTemplate
			}
			
			component MyComponent is MyType {
			}
		'''

		val tcl = '''
			package com.examples
			
			# UniCodeTest
			
			* Some test step
			Component: MyComponent
			- Übersicht der Verträge "Gebäudeschaden" λ
		'''

		// when
		val amlModel = aml.parseAml
		val tclModel = tcl.parseTcl('UniCodeTest.tcl')

		// then
		amlModel.assertNoErrors
		tclModel => [
			assertNoErrors
			val testStep = test.steps.head.contexts.head.steps.head as TestStep
			testStep.interaction => [
				assertNotNull('Interaction not found, linking template to fixture method failed.')
				defaultMethod.operation.qualifiedName.assertEquals(DummyFixture.canonicalName+'.startApplication')
			]
		]
	}

}
