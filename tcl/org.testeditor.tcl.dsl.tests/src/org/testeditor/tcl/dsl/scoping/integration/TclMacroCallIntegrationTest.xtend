package org.testeditor.tcl.dsl.scoping.integration

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable
import org.junit.Before
import org.junit.Test
import org.mockito.Mock
import org.testeditor.tcl.dsl.jvmmodel.AbstractTclGeneratorIntegrationTest
import org.testeditor.tcl.dsl.jvmmodel.TclJvmModelInferrer

import static org.mockito.Matchers.*
import static org.mockito.Mockito.*

class TclMacroCallIntegrationTest extends AbstractTclGeneratorIntegrationTest {

	@Inject TclJvmModelInferrer jvmModelInferrer // class under test
	@Mock ITreeAppendable outputStub

	@Before
	def void setUp() {
		when(outputStub.trace(any(EObject))).thenReturn(outputStub)
		when(outputStub.append(any(CharSequence))).thenReturn(outputStub)
	}

	@Test
	def void testMacroParameterResolutionThroughCallChains() {
		// given
		// aml description to make use of a "Dummy" component and its single interaction "start"
		parseAml('''
			package org.test
			
			import org.testeditor.dsl.common.testing.*
			
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
		parseTcl('''
			package org.test
			
			import org.test.*
			
			# MacroCollection
			
			## MacroOther
			template = "other" ${op}
			Macro: MacroCollection
			- something @op
			
			## MacroSomething
			template = "something" ${param}
			Component: Dummy
			- start @param
		''')

		// tcl that uses the macro(template) "other" with parameter "MyApp"
		val tcl = parseTcl('''
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
