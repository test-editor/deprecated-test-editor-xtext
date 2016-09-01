package org.testeditor.fixture.assertion;

import org.junit.Assert;
import org.testeditor.fixture.core.interaction.FixtureMethod;

public class AssertionFixture {

	@FixtureMethod
	public void assertContains(String input, String substring) {
		if (!input.contains(substring)) {
			Assert.fail(String.format("Expected to find substring='%s' in input='%s'.", substring, input));
		}
	}
	
}
