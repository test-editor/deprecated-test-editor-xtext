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
		val aml = '''
			package com.example
			
					interaction type executeContextMenuEntry {
						label = " execute context menu entry"
						template = "execute menu item " ${item} " in tree" ${element} 
						method = SWTFixture.executeContextMenuEntry(element,item)
					}
					
					element type TreeView {
						interactions =  executeContextMenuEntry
					}
					
					value-space projectmenues = #[ "New", "Open" ]
					
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
		var tcl = '''
			package com.example
			
			# Test
			* Start the famous greetings application
			Component: ProjectExplorer
			- execute menu item  "New" in tree <ProjektBaum>
		'''

		val resourceSet = resourceSetProvider.get
		amlParser.parse(aml, URI.createURI("swt.aml"), resourceSet)

		var model = parser.parse(tcl, URI.createURI("Test.tcl"), resourceSet)
		validator.assertNoIssues(model)

		tcl = '''
			package com.example
			
			# Test2
			* Start the famous greetings application
			Component: ProjectExplorer
			- execute menu item  "Neww"  in tree <ProjektBaum>
		'''
		model = parser.parse(tcl, URI.createURI("Test2.tcl"), resourceSet)
		assertFalse(validator.validate(model).isEmpty)
	}

}
