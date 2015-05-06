package org.testeditor.xmllibrary.dsl.importer

import com.google.inject.Guice
import org.junit.Before

/**
 * Enables dependency injection for tests.
 */
abstract class AbstractImporterTest {
	
	@Before
	def void setup() {
		val injector = Guice.createInjector(new ImporterModule)
		injector.injectMembers(this)
	}
	
}