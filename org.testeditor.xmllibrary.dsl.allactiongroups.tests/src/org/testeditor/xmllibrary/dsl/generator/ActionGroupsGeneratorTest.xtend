package org.testeditor.xmllibrary.dsl.generator

import javax.inject.Inject
import org.junit.Assert
import org.junit.Test
import org.testeditor.xmllibrary.dsl.tests.AbstractActionGroupsTest
import org.testeditor.xmllibrary.model.ModelFactory

class ActionGroupsGeneratorTest extends AbstractActionGroupsTest {

	static val model = ModelFactory.eINSTANCE

	@Inject
	AllActionGroupsDslGenerator generator

	@Test
	def void simpleTest() {

		// Given
		val input = model.createActionGroups => [
			actionGroups += model.createActionGroup => [
				name = "Allgemein Browser"
				actions += model.createAction => [
					technicalBindingType = model.createTechnicalBindingType => [
						id = "Pruefe_Text_Nicht_In_Element"
						name = "Ist Text am Element nicht vorhanden"
					]
					actionName = model.createActionName => [
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

}
