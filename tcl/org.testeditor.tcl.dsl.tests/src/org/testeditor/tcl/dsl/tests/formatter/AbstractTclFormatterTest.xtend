package org.testeditor.tcl.dsl.tests.formatter

import javax.inject.Inject
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import com.google.inject.Provider
import org.eclipse.xtext.junit4.formatter.FormatterTestRequest
import org.eclipse.xtext.junit4.formatter.FormatterTester

class AbstractTclFormatterTest extends AbstractTclTest {
	@Inject Provider<FormatterTestRequest> formatterRequestProvider
	@Inject protected FormatterTester formatterTester

}
