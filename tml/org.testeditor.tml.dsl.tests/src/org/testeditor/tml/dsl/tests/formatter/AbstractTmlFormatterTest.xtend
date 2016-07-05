package org.testeditor.tml.dsl.tests.formatter

import javax.inject.Inject
import org.eclipse.xtext.junit4.formatter.FormatterTester
import org.testeditor.tml.dsl.tests.AbstractTmlTest

class AbstractTmlFormatterTest extends AbstractTmlTest {
	@Inject extension protected FormatterTester formatterTester

}
