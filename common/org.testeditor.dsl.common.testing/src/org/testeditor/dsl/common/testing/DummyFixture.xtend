package org.testeditor.dsl.common.testing

import org.testeditor.fixture.core.interaction.FixtureMethod

public class DummyFixture {

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