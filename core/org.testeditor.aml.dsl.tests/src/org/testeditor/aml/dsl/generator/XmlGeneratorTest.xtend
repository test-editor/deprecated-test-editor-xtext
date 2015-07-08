package org.testeditor.aml.dsl.generator

import javax.inject.Inject
import org.junit.Test

/** 
 * Tests for {@link XmlGenerator} 
 */
class XmlGeneratorTest extends AbstractGeneratorTest {
	
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