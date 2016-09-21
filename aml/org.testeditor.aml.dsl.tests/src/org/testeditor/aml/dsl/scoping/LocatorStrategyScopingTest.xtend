package org.testeditor.aml.dsl.scoping

import java.util.Collection
import java.util.List
import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EReference
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameter
import org.junit.runners.Parameterized.Parameters
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.InteractionType
import org.testeditor.aml.dsl.tests.parser.AbstractParserTest
import org.testeditor.dsl.common.testing.DummyLocatorStrategy

import static org.testeditor.aml.AmlPackage.Literals.*

@RunWith(Parameterized)
class LocatorStrategyScopingTest extends AbstractParserTest {

	/**
	 * Description of the test data.
	 * 
	 * parameter 0: test name - gives the test a name and is for documentation only
	 * parameter 1: import section - the import section that should be applied to the AML
	 * parameter 2: expectedScope - a list of qualified names that are expected to be in scope 
	 */
	@Parameters(name="{index}: {0}")
	def static Collection<Object[]> data() {
		return #[
			#['empty scope', '', #[]],
			#['non static import ', '''
				import «DummyLocatorStrategy.name»
			'''.toString, #[
				"DummyLocatorStrategy.SINGLE",
				"DummyLocatorStrategy.ID",
				"DummyLocatorStrategy.LABEL"
			]],
			#['static import', '''
				import static «DummyLocatorStrategy.name».*
			'''.toString, #[
				"SINGLE",
				"ID",
				"LABEL"
			]],
			#['static and non static import', '''
				import «DummyLocatorStrategy.name»
				import static «DummyLocatorStrategy.name».*
			'''.toString, #[
				"SINGLE",
				"ID",
				"LABEL",
				"DummyLocatorStrategy.SINGLE",
				"DummyLocatorStrategy.ID",
				"DummyLocatorStrategy.LABEL"
			]]
		]
	}

	@Inject AmlScopeProvider scopeProvider

	@Parameter(value=0)
	public String testCaseName

	@Parameter(value=1)
	public String importSection

	@Parameter(value=2)
	public List<String> expectedScope

	@Test
	def void testInteractionTypeScope() {
		// given
		val input = '''
			«importSection»
			
			interaction type doSomething
		'''
		val interactionType = input.parseAmlWithStdPackage(InteractionType)
		interactionType.assertNoSyntaxErrors

		// when
		val scope = getQualifiedNamesInScope(interactionType, INTERACTION_TYPE__LOCATOR_STRATEGY)

		// then
		scope.assertEquals(expectedScope)
	}

	@Test
	def void testComponentElementScope() {
		// given
		val input = '''
			«importSection»
			
			component type Dialog
			component MyDialog is Dialog {
				element Ok is Button
			}
			element type Button
		'''
		val componentElement = input.parseAmlWithStdPackage(ComponentElement)
		componentElement.assertNoSyntaxErrors

		// when
		val scope = getQualifiedNamesInScope(componentElement, COMPONENT_ELEMENT__LOCATOR_STRATEGY)

		// then
		scope.assertEquals(expectedScope)
	}

	private def List<String> getQualifiedNamesInScope(EObject context, EReference reference) {
		val scope = scopeProvider.getScope(context, reference)
		return scope.allElements.map[qualifiedName.toString].toList
	}

}
