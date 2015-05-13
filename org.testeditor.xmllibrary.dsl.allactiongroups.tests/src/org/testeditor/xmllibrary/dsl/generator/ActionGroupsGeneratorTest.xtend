package org.testeditor.xmllibrary.dsl.generator

import javax.inject.Inject
import org.junit.Assert
import org.junit.Test
import org.testeditor.xmllibrary.dsl.tests.AbstractActionGroupsTest
import org.testeditor.xmllibrary.model.ModelFactory

import static org.testeditor.xmllibrary.model.ActionType.*

class ActionGroupsGeneratorTest extends AbstractActionGroupsTest {

	static val model = ModelFactory.eINSTANCE

	@Inject
	AllActionGroupsDslGenerator generator

	@Test
	def void actionGroupWithOneAction() {

		// Given
		val input = model.createActionGroups => [
			actionGroups += model.createActionGroup => [
				name = "Allgemein Browser"
				actions += model.createAction => [
					technicalBindingType = model.createTechnicalBindingType => [
						id = "Pruefe_Text_Nicht_In_Element"
						name = "Ist Text am Element nicht vorhanden"
					]
					actionNames += model.createActionName => [
						locator = "name_id"
						name = "Name"
					]
				]
			]
		]

		// When
		val result = generator.compile(input, null)

		// Then
		println(result)

		val expected = '''
			<?xml version="1.0" encoding="UTF-8"?>
			<ActionGroups xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" schemaVersion="1.1">
				<ActionGroup name="Allgemein Browser">
					<action technicalBindingType="Pruefe_Text_Nicht_In_Element">
						<actionName locator="name_id">Name</actionName>
					</action>
				</ActionGroup>
			</ActionGroups>
		'''

		Assert.assertEquals(expected, result)
	}

	@Test
	def void actionGroupWithTwoActionsForSameTechnicalBinding() {

		// Given
		val input = model.createActionGroups => [
			actionGroups += model.createActionGroup => [
				name = "Allgemein Browser"
				actions += model.createAction => [
					technicalBindingType = model.createTechnicalBindingType => [
						id = "Pruefe_Text_Nicht_In_Element"
						name = "Ist Text am Element nicht vorhanden"
					]
					actionNames += model.createActionName => [
						locator = "name_id"
						name = "Name"
					]
					actionNames += model.createActionName => [
						locator = "name_id_2"
						name = "Name_2"
					]
				]
			]
		]

		// When
		val result = generator.compile(input, null)

		// Then
		println(result)

		val expected = '''
			<?xml version="1.0" encoding="UTF-8"?>
			<ActionGroups xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" schemaVersion="1.1">
				<ActionGroup name="Allgemein Browser">
					<action technicalBindingType="Pruefe_Text_Nicht_In_Element">
						<actionName locator="name_id">Name</actionName>
					</action>
					<action technicalBindingType="Pruefe_Text_Nicht_In_Element">
						<actionName locator="name_id_2">Name_2</actionName>
					</action>
				</ActionGroup>
			</ActionGroups>
		'''

		Assert.assertEquals(expected, result)
	}

	@Test
	def void actionGroupWithActionAndArgumentList() {

		// Given
		val input = model.createActionGroups => [
			actionGroups += model.createActionGroup => [
				name = "Allgemein Browser"
				actions += model.createAction => [
					technicalBindingType = model.createTechnicalBindingType => [
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
					actionNames += model.createActionName => [
						locator = "land"
						name = "Land"
						argument = model.createArgument => [
							actionPart = model.createActionPart => [
								type = ARGUMENT
								id = "argument1"
							]
							values += #["Deutschland", "Italien", "USA"]
						]
					]
				]
			]
		]

		// When
		val result = generator.compile(input, null)

		// Then
		println(result)

		val expected = '''
			<?xml version="1.0" encoding="UTF-8"?>
			<ActionGroups xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" schemaVersion="1.1">
				<ActionGroup name="Allgemein Browser">
					<action technicalBindingType="Auswahl_Wert">
						<actionName locator="land">Land</actionName>
						<argument id="argument1">
							<value>Deutschland</value>
							<value>Italien</value>
							<value>USA</value>
						</argument>
					</action>
				</ActionGroup>
			</ActionGroups>
		'''

		Assert.assertEquals(expected, result)
	}
	
	@Test
	def void actionGroupWithActionAndArgumentListAndAction() {

		// Given
		val input = model.createActionGroups => [
			actionGroups += model.createActionGroup => [
				name = "Allgemein Browser"
				actions += model.createAction => [
					technicalBindingType = model.createTechnicalBindingType => [
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
					actionNames += model.createActionName => [
						locator = "land"
						name = "Land"
						argument = model.createArgument => [
							actionPart = model.createActionPart => [
								type = ARGUMENT
								id = "argument1"
							]
							values += #["Deutschland", "Italien", "USA"]
						]
					]
					actionNames += model.createActionName => [
						locator = "name"
						name = "Name"
					]					
				]
			]
		]

		// When
		val result = generator.compile(input, null)

		// Then
		println(result)

		val expected = '''
			<?xml version="1.0" encoding="UTF-8"?>
			<ActionGroups xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" schemaVersion="1.1">
				<ActionGroup name="Allgemein Browser">
					<action technicalBindingType="Auswahl_Wert">
						<actionName locator="land">Land</actionName>
						<argument id="argument1">
							<value>Deutschland</value>
							<value>Italien</value>
							<value>USA</value>
						</argument>
					</action>
					<action technicalBindingType="Auswahl_Wert">
						<actionName locator="name">Name</actionName>
					</action>
				</ActionGroup>
			</ActionGroups>
		'''

		Assert.assertEquals(expected, result)
	}
	

}
