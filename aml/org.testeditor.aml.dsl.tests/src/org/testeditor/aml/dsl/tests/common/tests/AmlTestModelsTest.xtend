package org.testeditor.aml.dsl.tests.common.tests

import org.junit.Test
import org.testeditor.dsl.common.testing.DummyFixture

import static extension org.junit.Assert.*

class AmlTestModelsTest {

	@Test
	def testExistenceOfDummyFixtureMethod() {
		DummyFixture.methods.exists[name == "startApplication"].assertNotNull
	}

}
