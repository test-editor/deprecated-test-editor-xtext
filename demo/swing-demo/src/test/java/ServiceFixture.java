package org.testeditor.demo.swing;

import org.testeditor.fixture.core.interaction.FixtureMethod;

public class ServiceFixture {

	@FixtureMethod
	public String callService(String name) {
		return new GreetingApplication().sayHello();
	}
}
