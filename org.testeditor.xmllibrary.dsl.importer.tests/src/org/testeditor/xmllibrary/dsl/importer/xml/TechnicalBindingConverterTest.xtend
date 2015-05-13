package org.testeditor.xmllibrary.dsl.importer.xml

import com.google.inject.Inject
import org.junit.Test
import org.testeditor.xmllibrary.dsl.importer.AbstractImporterTest
import org.testeditor.xmllibrary.model.ActionPart

import static org.junit.Assert.*
import static org.testeditor.xmllibrary.model.ActionType.*

/**
 * Tests the conversion of technical bindings.
 */
class TechnicalBindingConverterTest extends AbstractImporterTest {

	@Inject TechnicalBindingConverter converter
	@Inject SampleData sampleData

	@Test
	def void testConvert() {
		// Given
		val bindings = sampleData.bindings

		// When
		val result = converter.convert(bindings)
		
		// Then
		result.types.findFirst[id == "Auswahl_Wert"] => [
			assertEquals("Wert auswählen", name)

			// Check action parts
			assertEquals(5, actionParts.size)
			actionParts.forEach [ part, i | assertEquals(0, part.position)]
			actionParts.get(0).assertText("wähle aus der Combobox")
			actionParts.get(1).assertActionName
			actionParts.get(2).assertText("den Wert")
			actionParts.get(3).assertArgument("argument1")
			actionParts.get(4).assertText("aus")
		]
	}

	protected def void assertText(ActionPart part, String value) {
		assertEquals(TEXT, part.type)
		assertEquals(value, part.value)
	}

	protected def void assertArgument(ActionPart part, String id) {
		assertEquals(ARGUMENT, part.type)
		assertEquals(id, part.id)
	}

	protected def void assertActionName(ActionPart part) {
		assertEquals(ACTION_NAME, part.type)
	}

}