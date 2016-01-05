package org.testeditor.tcl.dsl.jvmmodel

import java.util.ArrayList
import java.util.List
import org.testeditor.fixture.core.interaction.FixtureMethod

class DummyFixture {
	@FixtureMethod
	def void startApplication(String name) {
	}

	@FixtureMethod
	def void stopApplication() {
	}

	@FixtureMethod
	def String getValue(String locator) {
		return "";
	}

	@FixtureMethod
	def void setValue(String locator, String value) {
	}
	
    @FixtureMethod
    def List<?> getList(String locator) {
    	return new ArrayList<Object>();
    }
}