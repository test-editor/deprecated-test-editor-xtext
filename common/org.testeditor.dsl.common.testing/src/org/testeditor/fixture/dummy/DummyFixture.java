package org.testeditor.fixture.dummy;

import org.testeditor.fixture.core.interaction.FixtureMethod;

/**
 * This is a dummy fixture for usage with test cases.
 */
public class DummyFixture {

    @FixtureMethod
    public void someFixtureMethod() {
    }

    public void someUnrelatedMethod() {
    }

    @FixtureMethod
    public void startApplication(String name) {
    }

    @FixtureMethod
    public void stopApplication() {
    }

    @FixtureMethod
    public String getValue(String locator) {
        return "";
    }

    @FixtureMethod
    public void setValue(String locator, String value) {
    }

}
