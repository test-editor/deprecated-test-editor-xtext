package org.testeditor.xmllibrary.dsl.generator

import javax.inject.Inject
import org.junit.Test
import org.testeditor.xmllibrary.dsl.tests.AbstractTechnicalBindingTest
import org.testeditor.xmllibrary.model.ModelFactory
import static org.testeditor.xmllibrary.model.ActionType.*
import org.junit.Assert

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
		val result = generator.compile(input, null)
		
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
	
}