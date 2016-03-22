package org.testeditor.tcl.dsl.validation

import com.google.inject.Provider
import javax.inject.Inject
import org.eclipse.emf.common.util.URI
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.resource.XtextResourceSet
import org.junit.Before
import org.junit.Test
import org.testeditor.aml.AmlModel
import org.testeditor.aml.dsl.AmlStandaloneSetup
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest

class TclValidatorTest extends AbstractParserTest {

	@Inject
	private Provider<XtextResourceSet> resourceSetProvider;

	protected ParseHelper<AmlModel> amlParser

	@Inject
	ValidationTestHelper validator;

	@Before
	def void initializeAmlParseHelper() {
		val amlInjector = new AmlStandaloneSetup().createInjectorAndDoEMFRegistration
		amlParser = amlInjector.getInstance(ParseHelper)
	}

	@Test
	def void validateStringArray() {
		//given
		val aml = getAMLWithValueSpace('''#[ "New", "Open" ]''')
		var tcl = getTCLWithValue("Test", "New")

		val resourceSet = resourceSetProvider.get
		amlParser.parse(aml, URI.createURI("swt.aml"), resourceSet)
		var tclError = getTCLWithValue("Test2", "Save")
		
		//when
		var model = parser.parse(tcl, URI.createURI("Test.tcl"), resourceSet)
		var modelError = parser.parse(tclError, URI.createURI("Test2.tcl"), resourceSet)
		
		//then
		validator.assertNoIssues(model)
		assertFalse(validator.validate(modelError).isEmpty)
	}

	@Test
	def void validateNumberRange() {
		//given
		val aml = getAMLWithValueSpace("2 ... 5")
		var tcl = getTCLWithValue("Test", "4")

		val resourceSet = resourceSetProvider.get
		amlParser.parse(aml, URI.createURI("swt.aml"), resourceSet)
		var tclError = getTCLWithValue("Test2", "1")
		
		//when
		var model = parser.parse(tcl, URI.createURI("Test.tcl"), resourceSet)
		var modelError = parser.parse(tclError, URI.createURI("Test2.tcl"), resourceSet)

		//then
		validator.assertNoIssues(model)
		assertFalse(validator.validate(modelError).isEmpty)
	}

	@Test
	def void validateRegEx() {
		// given
		val aml = getAMLWithValueSpace('''"^[a-zA-Z_0-9]"''')
		var tcl = getTCLWithValue("Test", "h")

		val resourceSet = resourceSetProvider.get
		amlParser.parse(aml, URI.createURI("swt.aml"), resourceSet)
		var tclError = getTCLWithValue("Test2", "!!hello")

		//when
		var model = parser.parse(tcl, URI.createURI("Test.tcl"), resourceSet)
		var modelError = parser.parse(tclError, URI.createURI("Test2.tcl"), resourceSet)

		//then
		validator.assertNoIssues(model)
		assertFalse(validator.validate(modelError).isEmpty)
	}

	def CharSequence getTCLWithValue(String testName, String value) { '''
			package com.example
			
			# «testName»
			* Start the famous greetings application
			Component: ProjectExplorer
			- execute menu item  "«value»"  in tree <ProjektBaum>
		'''
	}

	def CharSequence getAMLWithValueSpace(String valuespace) {'''
			package com.example
			
			interaction type executeContextMenuEntry {
				label = " execute context menu entry"
				template = "execute menu item " ${item} " in tree" ${element} 
				method = SWTFixture.executeContextMenuEntry(element,item)
			}
			
			element type TreeView {
				interactions =  executeContextMenuEntry
			}
			
			value-space projectmenues = «valuespace» 
			
			component type General {
			}
			
			
			component ProjectExplorer is General {
				element ProjektBaum is TreeView {
					label = "Projekt Baum"
					locator ="Project Explorer"
					executeContextMenuEntry.item restrict to projectmenues 
				}
			}
		'''
	}

}
