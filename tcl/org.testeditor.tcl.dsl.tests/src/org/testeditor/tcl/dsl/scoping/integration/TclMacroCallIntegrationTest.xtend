package org.testeditor.tcl.dsl.scoping.integration

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable
import org.junit.Test
import org.mockito.Mock
import org.testeditor.tcl.dsl.jvmmodel.AbstractTclGeneratorIntegrationTest
import org.testeditor.tcl.dsl.jvmmodel.TclJvmModelInferrer

import static org.mockito.Matchers.*
import static org.mockito.Mockito.*

class TclMacroCallIntegrationTest extends AbstractTclGeneratorIntegrationTest {

	@Inject TclJvmModelInferrer jvmModelInferrer // class under test
	@Mock ITreeAppendable outputStub

	override void setup() {
		super.setup
		when(outputStub.trace(any(EObject))).thenReturn(outputStub)
		when(outputStub.append(any(CharSequence))).thenReturn(outputStub)
	}

	private def parseAmlModel(String aml) {
		val amlModel = amlParseHelper.parse(aml, resourceSet).assertNoSyntaxErrors
		return amlModel
	}

	private def parseTmlModel(String tml) {
		val tmlModel = tmlParseHelper.parse(tml, resourceSet).assertNoSyntaxErrors
		return tmlModel
	}

	private def parseTclModel(String tcl) {
		return tclParseHelper.parse(tcl, resourceSet).assertNoSyntaxErrors
	}

	@Test
	def void testMacroParameterResolutionThroughCallChains() {
		// given
		// aml description to make use of a "Dummy" component and its single interaction "start"
		parseAmlModel('''
			package org.test
			
			import org.testeditor.tcl.dsl.jvmmodel.*
			
			interaction type start {
				template = "start" ${appname}
				method = DummyFixture.startApplication(appname)
			}
			
			component type DummyCT {
				interactions = start
			}
			
			component Dummy is DummyCT {}						
		''')

		// macro definitions such that "other" is a macro again which uses "something" which uses an aml component
		parseTmlModel('''
			package org.test
			
			import org.test.*
			
			# MacroCollection
			
			template = "other" ${op}
			Macro: MacroCollection
			- something @op
			
			template = "something" ${param}
			Component: Dummy
			- start @param
		''')

		// tcl that uses the macro(template) "other" with parameter "MyApp"
		val tcl = parseTclModel('''
			package org.test
			
			# Test
			
			* my test 
			Macro: MacroCollection
			- other "MyApp"
		''')

		// when
		jvmModelInferrer.generateMethodBody(tcl.test, outputStub)

		// then
		// expectation is tcl: calls macro other("MyApp"), which again calls macro start("MyApp"), which again calls aml start("MyApp"), which again calls aml-method startApplication("MyApp")
		// ... and that the parameter reference @... within the macro is correctly resolved through this call chain
		verify(outputStub).append('dummyFixture.startApplication("MyApp");')
	}

}
