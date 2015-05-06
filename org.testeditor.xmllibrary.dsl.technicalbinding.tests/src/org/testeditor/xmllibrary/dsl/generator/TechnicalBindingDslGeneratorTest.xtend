package org.testeditor.xmllibrary.dsl.generator

import javax.inject.Inject
import org.junit.Test
import org.testeditor.xmllibrary.dsl.tests.AbstractTechnicalBindingTest
import org.testeditor.xmllibrary.model.ModelFactory

class TechnicalBindingDslGeneratorTest extends AbstractTechnicalBindingTest {
	
	static val model = ModelFactory.eINSTANCE
	
	@Inject
	TechnicalBindingDslGenerator generator
	
	@Test
	def void blubb() {
		// Given
		val input = model.createTechnicalBindingTypes => [
			types += model.createTechnicalBindingType => [
				id = "Test"
				name = "This is a test"
			]
		]
		
		// When
		val result = generator.compile(input, null)
		
		// Then
		println(result)
	}
	
}