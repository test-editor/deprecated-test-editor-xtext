package org.testeditor.tcl.dsl.tests.parser

import javax.inject.Inject
import org.eclipse.xtext.resource.XtextResourceSet
import org.junit.Test
import org.testeditor.tcl.util.ExampleAmlModel
import org.testeditor.tcl.util.TclModelUtil

class TclTslParserIntegrationTest extends AbstractParserTest {

	@Inject ExampleAmlModel amlModel

	@Inject extension TclModelUtil

	@Test
	def void testUnicodeTslSpecStepsWithinTcl() {
		resourceSet = amlModel.model.eResource.resourceSet as XtextResourceSet
		// given	
		val tcl = '''
			package com.example
			
			/*
				some multiline comment that is allowed
		     */
			
			# ExampleTest
			
			* Simple spec with a * in it
			* တက်စတ တက်စတ. တက်စတ@öttö.de. တက်စတ
			Component: MyDialog
			- Enter month "10" and year "2000" into <Date>
			// - Enter month "20" and year "2000" into <Date>
			- Enter month "30" and year "2000" into <Date>
			
			* !@#$%^&*()-_=+`~\|][}{;:''""<>,./? with ξεσκεπάζω την ξεσκεπάζω την Sævör grét áðan því úlpan var ónýt.
			* he\'s not very cooperative 'param'
			* he\"s not ccop. right? "param" and more
		'''

		tcl.parseTcl('ExampleTest.tcl') => [
			assertNoErrors
			test.steps.assertSize(5) => [
				get(0).contents.restoreString.assertEquals('Simple spec with a * in it')
				get(1).contents.restoreString.assertEquals('တ က ် စ တ တ က ် စ တ . တ က ် စ တ @ ö tt ö . de . တ က ် စ တ')
				get(2).contents.restoreString.assertEquals('! @ # $ % ^ & * ( ) - _ = + ` ~ \\ | ] [ } { ; : "" "" <> , . / ? with ξ ε σ κ ε π ά ζ ω τ η ν ξ ε σ κ ε π ά ζ ω τ η ν S æ v ö r gr é t á ð an þ v í ú lpan var ó n ý t .')
				get(3).contents.restoreString.assertEquals("he \\' s not very cooperative \"param\"")
				get(4).contents.restoreString.assertEquals("he \\\" s not ccop . right ? \"param\" and more")
			]
		]
	}

}
