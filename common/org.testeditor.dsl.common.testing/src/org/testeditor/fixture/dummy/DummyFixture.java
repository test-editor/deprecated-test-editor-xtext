package org.testeditor.fixture.dummy;

import org.testeditor.fixture.core.interaction.FixtureMethod;

/**
 * This is a dummy fixture for usage with test cases.
 */
public class DummyFixture {

    @FixtureMethod
    public void someFixtureMethod() {
    }
    
    @FixtureMethod
    public String getValue() {
        return "";
    }
    
    @FixtureMethod
    public void setValue(String value) {
    }
    
    public void someUnrelatedMethod() {
    }
    
}
