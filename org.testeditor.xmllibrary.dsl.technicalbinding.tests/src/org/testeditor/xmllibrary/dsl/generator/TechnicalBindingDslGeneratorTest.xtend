package org.testeditor.xmllibrary.dsl.generator

import javax.inject.Inject
import org.junit.Assert
import org.junit.Test
import org.testeditor.xmllibrary.dsl.tests.AbstractTechnicalBindingTest
import org.testeditor.xmllibrary.model.ModelFactory

import static org.testeditor.xmllibrary.model.ActionType.*

class TechnicalBindingDslGeneratorTest extends AbstractTechnicalBindingTest {

	static val model = ModelFactory.eINSTANCE

	@Inject
	TechnicalBindingDslGenerator generator

	@Test
	def void simpleTest() {

		// Given
		val input = model.createTechnicalBindingTypes => [
			types += model.createTechnicalBindingType => [
				id = "Pruefe_Text_Nicht_In_Element"
				name = "Ist Text am Element nicht vorhanden"
				actionParts += model.createActionPart => [
					value = "überprüfe ob am Element"
				]
				actionParts += model.createActionPart => [
					type = ACTION_NAME
				]
				actionParts += model.createActionPart => [
					value = "der Text"
				]
				actionParts += model.createActionPart => [
					type = ARGUMENT
				]
				actionParts += model.createActionPart => [
					value = "nicht vorhanden ist"
				]
			]
		]

		// When
		val result = generator.compile(input)

		// Then
		println(result)

		val expected = '''
			<?xml version="1.0" encoding="UTF-8" standalone="no"?>
			<TechnicalBindingTypes xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" schemaVersion="1.1">
				<TechnicalBindingType id="Pruefe_Text_Nicht_In_Element" name="Ist Text am Element nicht vorhanden">
					<actionPart position="1" type="TEXT" value="überprüfe ob am Element"/>
					<actionPart position="2" type="ACTION_NAME"/>
					<actionPart position="3" type="TEXT" value="der Text"/>
					<actionPart position="4" type="ARGUMENT"/>
					<actionPart position="5" type="TEXT" value="nicht vorhanden ist"/>
				</TechnicalBindingType>
			</TechnicalBindingTypes>
		'''

		Assert.assertEquals(expected, result)
	}

	@Test
	def void technicalBindingWithArgumentList() {

		// Given
		val input = model.createTechnicalBindingTypes => [
			types += model.createTechnicalBindingType => [
				id = "Auswahl_Wert"
				name = "Wert auswählen"
				actionParts += model.createActionPart => [
					value = "wähle aus der Combobox"
				]
				actionParts += model.createActionPart => [
					type = ACTION_NAME
				]
				actionParts += model.createActionPart => [
					value = "den Wert"
				]
				actionParts += model.createActionPart => [
					type = ARGUMENT
					id = "argument1"
				]
				actionParts += model.createActionPart => [
					value = "aus"
				]
			]
		]

		// When
		val result = generator.compile(input)

		// Then
		println(result)

		val expected = '''
			<?xml version="1.0" encoding="UTF-8" standalone="no"?>
			<TechnicalBindingTypes xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" schemaVersion="1.1">
				<TechnicalBindingType id="Auswahl_Wert" name="Wert auswählen">
					<actionPart position="1" type="TEXT" value="wähle aus der Combobox"/>
					<actionPart position="2" type="ACTION_NAME"/>
					<actionPart position="3" type="TEXT" value="den Wert"/>
					<actionPart position="4" type="ARGUMENT" id="argument1"/>
					<actionPart position="5" type="TEXT" value="aus"/>
				</TechnicalBindingType>
			</TechnicalBindingTypes>
		'''

		Assert.assertEquals(expected, result)
	}

}
