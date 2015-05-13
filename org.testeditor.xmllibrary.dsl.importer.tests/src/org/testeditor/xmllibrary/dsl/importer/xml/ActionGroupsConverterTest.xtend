package org.testeditor.xmllibrary.dsl.importer.xml

import com.google.inject.Inject
import java.util.List
import org.junit.Test
import org.testeditor.xmllibrary.dsl.importer.AbstractImporterTest
import org.testeditor.xmllibrary.model.Action

import static org.junit.Assert.*

/**
 * Tests the conversion of action groups.
 */
class ActionGroupsConverterTest extends AbstractImporterTest {

	@Inject TechnicalBindingConverter bindingConverter
	@Inject ActionGroupsConverter converter
	@Inject SampleData sampleData
	
	@Test
	def void testConvert() {
		// Given
		val bindings = bindingConverter.convert(sampleData.bindings)
		val actionGroups = sampleData.actionGroups

		// When
		val result = converter.convert(actionGroups, bindings)

		// Then
		result.actionGroups.findFirst[name == "Lokale Anmeldung"] => [
			assertEquals(9, actions.size)
			actions.get(0) => [
				assertBinding("Eingabe_Wert")
				assertActionName("user", "Name")
			]
			actions.get(4) => [
				assertBinding("Auswahl_Wert")
				assertActionName("land", "Land")
				assertArgument("argument1", #[
					"Deutschland",
					"Italien",
					"USA",
					"Schweden",
					"Australien"
				])
			]
		]
	}
	
	def void assertBinding(Action action, String binding) {
		assertNotNull(action.technicalBindingType)
		assertEquals(binding, action.technicalBindingType.id)
	}
	
	def void assertActionName(Action action, String locator, String name) {
		assertNotNull(action.actionNames)
		assertTrue(action.actionNames.size == 1)
		val actionName = action.actionNames.head
		assertEquals(locator, actionName.locator)
		assertEquals(name, actionName.name)
	}
	
	def void assertArgument(Action action, String id, List<String> values) {
		assertNotNull(action.actionNames)
		assertTrue(action.actionNames.size == 1)
		val actionName = action.actionNames.head
		assertNotNull(actionName.argument)
		val argument = actionName.argument
		assertNotNull(argument.actionPart)
		assertEquals(id, argument.actionPart.id)
		val expectedValues = values.join("\n")
		val actualValues = argument.values.join("\n")
		assertEquals(expectedValues, actualValues)
	}

}