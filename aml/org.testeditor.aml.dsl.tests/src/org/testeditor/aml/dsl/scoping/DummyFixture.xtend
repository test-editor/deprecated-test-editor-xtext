package org.testeditor.aml.dsl.scoping

import org.testeditor.fixture.core.interaction.FixtureMethod

package class DummyFixture {

    @FixtureMethod
    def void someFixtureMethod() {
    }

    def void someUnrelatedMethod() {
    }

	@FixtureMethod
	def void startApplication(String name) {
	}

	@FixtureMethod
	def void setValue(String locator, String value) {
	}

}