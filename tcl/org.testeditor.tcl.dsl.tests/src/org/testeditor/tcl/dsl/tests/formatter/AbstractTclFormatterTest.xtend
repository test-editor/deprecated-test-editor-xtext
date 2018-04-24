package org.testeditor.tcl.dsl.tests.formatter

import javax.inject.Inject
import org.eclipse.xtext.testing.formatter.FormatterTestHelper
import org.eclipse.xtext.testing.formatter.FormatterTestRequest
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1
import org.testeditor.tcl.dsl.tests.AbstractTclTest

class AbstractTclFormatterTest extends AbstractTclTest {

	@Inject protected FormatterTestHelper formatterTester

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
