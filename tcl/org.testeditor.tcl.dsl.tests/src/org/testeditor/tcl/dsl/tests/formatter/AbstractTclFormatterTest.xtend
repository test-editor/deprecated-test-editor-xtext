package org.testeditor.tcl.dsl.tests.formatter

import javax.inject.Inject
import org.eclipse.xtext.junit4.formatter.FormatterTester
import org.testeditor.tcl.dsl.tests.AbstractTclTest

class AbstractTclFormatterTest extends AbstractTclTest {

	@Inject extension protected FormatterTester formatterTester

}
