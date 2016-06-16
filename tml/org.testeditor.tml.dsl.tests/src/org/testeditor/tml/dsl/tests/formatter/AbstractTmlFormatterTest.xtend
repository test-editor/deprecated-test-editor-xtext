package org.testeditor.tml.dsl.tests.formatter

import com.google.inject.Provider
import javax.inject.Inject
import org.eclipse.xtext.junit4.formatter.FormatterTestRequest
import org.eclipse.xtext.junit4.formatter.FormatterTester
import org.testeditor.tml.dsl.tests.AbstractTmlTest

class AbstractTmlFormatterTest extends AbstractTmlTest {
	@Inject Provider<FormatterTestRequest> formatterRequestProvider
	@Inject protected FormatterTester formatterTester

}
