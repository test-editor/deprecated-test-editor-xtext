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

class TclGenerationOfLocatorStrategyTests extends AbstractTclGeneratorIntegrationTest {

	@Inject TclJvmModelInferrer jvmModelInferrer // class under test
	@Mock ITreeAppendable outputStub

	private static val TCL_CLICK_ON_DUMMY_BUTTON = '''
		package org.test

		# Test

		* my test
		Component: Dummy
		- click on <DummyButton>
	'''

	override void setup() {
		super.setup
		when(outputStub.trace(any(EObject))).thenReturn(outputStub)
		when(outputStub.append(any(CharSequence))).thenReturn(outputStub)
	}

	private def parseAmlModel(String aml) {
		val amlModel = amlParseHelper.parse(aml, resourceSet).assertNoSyntaxErrors
		return amlModel
	}

	private def parseTclModel(String tcl) {
		return tclParseHelper.parse(tcl, resourceSet).assertNoSyntaxErrors
	}

	@Test
	def void testDefaultLocatorStrategyGeneration() {
		// given
		parseAmlModel('''
			package org.test

			import org.testeditor.dsl.common.testing.DummyLocatorStrategy
			import org.testeditor.dsl.common.testing.DummyFixture

			interaction type click {
				template = "click on" ${element}
				method = DummyFixture.clickOn(element, locatorStrategy)
				locatorStrategy = LABEL
			}

			element type Button {
				interactions = click
			}

			component type DummyCT { }

			component Dummy is DummyCT {
				element DummyButton is Button {
					locator = "ok"
				}
			}
		''')

		val tcl = parseTclModel(org.testeditor.tcl.dsl.scoping.integration.TclGenerationOfLocatorStrategyTests.TCL_CLICK_ON_DUMMY_BUTTON)

		// when
		jvmModelInferrer.generateMethodBody(tcl.test, outputStub, #{})

		// then
		verify(outputStub).append('dummyFixture.clickOn("ok", org.testeditor.dsl.common.testing.DummyLocatorStrategy.LABEL);')
	}

	@Test
	def void testOverrideDefaultLocatorStrategyGeneration() {
		// given
		parseAmlModel('''
			package org.test

			import org.testeditor.dsl.common.testing.DummyLocatorStrategy
			import org.testeditor.dsl.common.testing.DummyFixture

			interaction type click {
				template = "click on" ${element}
				method = DummyFixture.clickOn(element, locatorStrategy)
				locatorStrategy = SINGLE
			}

			element type Button {
				interactions = click
			}

			component type DummyCT { }

			component Dummy is DummyCT {
				element DummyButton is Button {
					locator = "OK_BUTTON_ID"
					locatorStrategy = ID
				}
			}
		''')

		val tcl = parseTclModel(org.testeditor.tcl.dsl.scoping.integration.TclGenerationOfLocatorStrategyTests.TCL_CLICK_ON_DUMMY_BUTTON)

		// when
		jvmModelInferrer.generateMethodBody(tcl.test, outputStub, #{})

		// then
		verify(outputStub).append('dummyFixture.clickOn("OK_BUTTON_ID", org.testeditor.dsl.common.testing.DummyLocatorStrategy.ID);')
	}

	@Test
	def void testWithoutDefaultLocatorStrategyGeneration() {
		// given
		parseAmlModel('''
			package org.test

			import org.testeditor.dsl.common.testing.DummyLocatorStrategy
			import org.testeditor.dsl.common.testing.DummyFixture

			interaction type click {
				template = "click on" ${element}
				method = DummyFixture.clickOn(element, locatorStrategy)
			}

			element type Button {
				interactions = click
			}

			component type DummyCT { }

			component Dummy is DummyCT {
				element DummyButton is Button {
					locator = "OK_BUTTON_ID"
					locatorStrategy = ID
				}
			}
		''')

		val tcl = parseTclModel(org.testeditor.tcl.dsl.scoping.integration.TclGenerationOfLocatorStrategyTests.TCL_CLICK_ON_DUMMY_BUTTON)

		// when
		jvmModelInferrer.generateMethodBody(tcl.test, outputStub, #{})

		// then
		verify(outputStub).append('dummyFixture.clickOn("OK_BUTTON_ID", org.testeditor.dsl.common.testing.DummyLocatorStrategy.ID);')
	}

}
