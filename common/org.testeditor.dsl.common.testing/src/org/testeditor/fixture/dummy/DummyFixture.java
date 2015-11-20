package org.testeditor.fixture.dummy;

import java.util.ArrayList;
import java.util.List;

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
    public List<?> getList(String locator) {
    	return new ArrayList<Object>();
    }
    
    @FixtureMethod
    public void setValue(String locator, String value) {
    }

}
