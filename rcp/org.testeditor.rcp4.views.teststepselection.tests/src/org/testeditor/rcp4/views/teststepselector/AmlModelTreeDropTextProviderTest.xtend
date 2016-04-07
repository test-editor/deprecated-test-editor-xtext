package org.testeditor.rcp4.views.teststepselector

import javax.inject.Inject
import org.junit.Test
import org.testeditor.aml.dsl.tests.AbstractTest
import org.testeditor.aml.impl.AmlFactoryImpl

class AmlModelTreeDropTextProviderTest extends AbstractTest {

	@Inject AmlModelTreeDropTextProvider dropTextProvider
	@Inject AmlFactoryImpl amlFactory

	@Test
	def void elementReplacementRestrictedToUiElementReferences() {
		// when
		val result = dropTextProvider.adjustTextBy(0, amlFactory.createComponentElement => [name = "Test"],
			"<MyTest> <element> element <noelement> <lement>")

		// then
		assertEquals(result, "<MyTest> <Test> element <noelement> <lement>")
	}

}
