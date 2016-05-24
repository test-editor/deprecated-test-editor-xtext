package org.testeditor.tml.dsl.validation

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
import org.testeditor.tml.dsl.tests.parser.AbstractParserTest

class TmlValidatorTest extends AbstractParserTest {

	@Inject protected Provider<XtextResourceSet> resourceSetProvider
	@Inject protected XtextResourceSet resourceSet
	@Inject ValidationTestHelper validator

	protected ParseHelper<AmlModel> amlParseHelper

	@Before
	def void setUp() {
		resourceSet = resourceSetProvider.get
		resourceSet.classpathURIContext = this
		val injector = (new AmlStandaloneSetup).createInjectorAndDoEMFRegistration
		amlParseHelper = injector.getInstance(ParseHelper)
	}

	@Test
	def void validateStringArray() {
		// given
		val aml = getAmlWithValueSpace('''#[ "New", "Open" ]''')
		var tml = getTmlWithValue("Test", "New")

		amlParseHelper.parse(aml, resourceSet)
		var tmlError = getTmlWithValue("Test2", "Save")

		// when
		var model = parser.parse(tml, resourceSet)
		var modelError = parser.parse(tmlError, resourceSet)

		// then
		validator.assertNoIssues(model)
		assertFalse(validator.validate(modelError).isEmpty)
	}

	@Test
	def void validateNumberRange() {
		// given
		val aml = getAmlWithValueSpace("2 ... 5")
		var tml = getTmlWithValue("Test", "4")

		amlParseHelper.parse(aml, resourceSet)
		var tmlError = getTmlWithValue("Test2", "1")

		// when
		var model = parser.parse(tml, URI.createURI("Test.tml"), resourceSet)
		var modelError = parser.parse(tmlError, URI.createURI("Test2.tml"), resourceSet)

		// then
		validator.assertNoIssues(model)
		assertFalse(validator.validate(modelError).isEmpty)
	}

	@Test
	def void validateRegEx() {
		// given
		val aml = getAmlWithValueSpace('''"^[a-zA-Z_0-9]"''')
		var tml = getTmlWithValue("Test", "h")

		amlParseHelper.parse(aml, resourceSet)
		var tmlError = getTmlWithValue("Test2", "!!hello")

		// when
		var model = parser.parse(tml, resourceSet)
		var modelError = parser.parse(tmlError, resourceSet)

		// then
		validator.assertNoIssues(model)
		assertFalse(validator.validate(modelError).isEmpty)
	}

	def CharSequence getTmlWithValue(String macroCollectionName, String value) {
		'''
			package com.example
			
			// import com.example.*
			
			# «macroCollectionName»
			
			template = "do something"
			Component: ProjectExplorer
			- execute menu item  "«value»"  in tree <ProjektBaum>
		'''
	}

	def CharSequence getAmlWithValueSpace(String valuespace) {
		'''
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
