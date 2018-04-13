package org.testeditor.tcl.dsl.tests.parser

import javax.inject.Inject
import org.eclipse.xtext.resource.XtextResourceSet
import org.junit.Before
import org.junit.Test
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.util.ExampleAmlModel
import org.testeditor.tcl.util.TclModelUtil

class TclTslParserIntegrationTest extends AbstractParserTest {

	@Inject ExampleAmlModel amlModel

	@Inject extension TclModelUtil

	@Before
	def void setupResourceSet() {
		resourceSet = amlModel.model.eResource.resourceSet as XtextResourceSet
	}

	@Test
	def void testUnicodeInReferencedTslSpecId() {
		// given
		val tslId = 'ÖttoIsÜberμm'
		val tclId = 'ÜbersichtDerVerträgeλ'
		val tsl = '''
			package com.example
			
			# «tslId»
			
			* Unicode Spec Step Ü (ω=25 °/s, 25 µm/s))
		'''

		val tcl = '''
			package com.example
			
			# «tclId» implements «tslId»
			
			* Unicode Spec Step Ü (ω=25 °/s, 25 µm/s))'''

		// when
		val tslModel = tsl.parseTsl(tslId + '.tsl')
		val tclModel = tcl.parseTcl(tclId + '.tcl')

		// then
		tslModel.assertNoErrors
		tclModel => [
			assertNoErrors
			test.specification.name.assertEquals(tslId)
			test.name.assertEquals(tclId)
		]
	}
	
	@Test
	def void simpleTest() {
		val tcl = '''
			package com.example
				   	
			# SimpleTest
			
			* MyId another id 
			  multiline
			  Component: MyDialog
			  - Enter month "10" and year "2000" into <Date>
			* next
			'''
			
		tcl.parseTcl('SimpleTest.tcl') => [
			assertNoErrors 
			test.steps.assertSize(2) => [
				get(0).contents.restoreString.assertEquals('MyId another id multiline')
				(get(0).contexts.assertSize(1).head as ComponentTestStepContext) => [
					component.name.assertEquals("MyDialog")
					(steps.head as TestStep).contents.restoreString.assertEquals(
						'Enter month "10" and year "2000" into <Date>')
				]
				get(1).contents.restoreString.assertEquals('next')
			]
		]		
	}

	@Test
	def void testUnicodeTslSpecStepsWithinTcl() {
		// given	
		val tcl = '''
			package com.example
			
			/*
				some multiline comment that is allowed */
			
			# ExampleTest
			
			* Simple spec with a * in it
			* တက်စတ တက်စတ. တက်စတ@öttö.de. တက်စတ
			Component: MyDialog
			- Enter month "10" and year "2000" into <Date>
			// - Enter month "20" and year "2000" into <Date>
			- Enter month "30" and year "2000" into <Date>
			
			* !@#$%^&*()-_=+`~\|][}{;:''""<>,./? with ξεσκεπάζω την ξεσκεπάζω την Sævör grét áðan því úlpan var ónýt.
			* he\'s not very cooperative 'param'
			* he\"s not ccop. right? "param" and more'''

		// when
		val tclModel = tcl.parseTcl('ExampleTest.tcl')

		// then
		tclModel => [
			assertNoErrors
			test.steps.assertSize(5) => [
				get(0).contents.restoreString.assertEquals('Simple spec with a * in it')
				get(1).contents.restoreString.assertEquals('တ က ် စ တ တ က ် စ တ . တ က ် စ တ @ öttö . de . တ က ် စ တ')
				get(2).contents.restoreString.assertEquals('! @ # $ % ^ & * ( ) - _ = + ` ~ \\ | ] [ } { ; : "" "" <> , . / ? with ξεσκεπάζω την ξεσκεπάζω την Sævör grét áðan því úlpan var ónýt .')
				get(3).contents.restoreString.assertEquals("he \\' s not very cooperative \"param\"")
				get(4).contents.restoreString.assertEquals("he \\\" s not ccop . right ? \"param\" and more")
			]
		]
	}

}
