package org.testeditor.tcl.dsl.tests.formatter

import javax.inject.Inject
import org.eclipse.xtext.testing.formatter.FormatterTestHelper
import org.eclipse.xtext.testing.formatter.FormatterTestRequest
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1
import org.testeditor.tcl.dsl.tests.AbstractTclTest

import static java.lang.System.lineSeparator

class AbstractTclFormatterTest extends AbstractTclTest {

	@Inject protected FormatterTestHelper formatterTester

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

	// modification to formatting request necessary to not use the serializer
	// that does not work well with own whitespace terminal definitions
	def void assertFormatted(Procedure1<FormatterTestRequest> init) {
		formatterTester.assertFormatted [
			init.apply(it)
			useNodeModel = true
			useSerializer = false
		]
	}

}
