package org.testeditor.aml.dsl.generator

import javax.inject.Inject
import org.junit.Test
import org.testeditor.aml.dsl.tests.AbstractTest
import org.testeditor.aml.model.ModelFactory

/** 
 * Tests for {@link XmlGenerator} 
 */
class XmlGeneratorTest extends AbstractTest {
	
	static val factory = ModelFactory.eINSTANCE
	
	@Inject XmlGenerator generator
	
	@Test
	def void testGetPackageFolder() {
		// Given
		val model = factory.createAmlModel => [
			package = "com.example"
		]
		
		// When
		val packageFolder = generator.getPackageFolder(model)
		
		// Then
		packageFolder.assertEquals("com/example/")
	}
	
}