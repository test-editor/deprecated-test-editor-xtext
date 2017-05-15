package org.testeditor.tcl.dsl.tests.formatter

import javax.inject.Inject
import org.eclipse.xtext.testing.formatter.FormatterTestHelper
import org.testeditor.tcl.dsl.tests.AbstractTclTest

import static java.lang.System.lineSeparator

class AbstractTclFormatterTest extends AbstractTclTest {

	@Inject extension protected FormatterTestHelper formatterTester

	/**
	 * Converts the passed String into a single line and replaces all whitespace sequences
	 * by a double space.
	 */
	protected def String toSingleLine(CharSequence input) {
		return input.toString.split(lineSeparator).join(' ').replaceAll('\\s+', '  ')
	}

	protected def String trim(CharSequence input) {
		return input.toString.trim
	}

}
