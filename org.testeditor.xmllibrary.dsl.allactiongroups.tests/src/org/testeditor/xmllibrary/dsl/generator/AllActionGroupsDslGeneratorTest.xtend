package org.testeditor.xmllibrary.dsl.generator

import javax.inject.Inject
import org.junit.Assert
import org.junit.Test
import org.testeditor.xmllibrary.dsl.tests.AbstractTechnicalBindingTest
import org.testeditor.xmllibrary.model.ModelFactory

class AllActionGroupsDslGeneratorTest extends AbstractTechnicalBindingTest {
	
	static val model = ModelFactory.eINSTANCE
	
	@Inject
	AllActionGroupsDslGenerator generator
	
	@Test
	def void simpleTest() {
		// Given
		val input = model.createActionGroups => [
		]
		
		// When
		val result = generator.compile(input)
		
		// Then
		println(result)
		
		val expected = '''
		'''
		
		Assert.assertEquals(expected, result)
	}
	
}